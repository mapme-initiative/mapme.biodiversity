#' Active Fire Polygon
#'
#' This resource is published by Fire Information for Resource Management System
#' (FIRMS) from NASA as Near Real Time (NRT) active fire data. The data is
#' collected from Moderate Resolution Imaging Spectroradiometer (MODIS) and the
#' Visible Infrared Imaging Radiometer Suite (VIIRS). The resource represents the
#' fire hotspot with lat/lon coordinates along with information on fire pixel brightness
#' temperature, and fire radiative power (frp). The data from MODIS is available from
#' 2000 to 2021 and that from VIIRS is only available for 2012-2021 year range.
#'
#' The data from the following instruments are available:
#' - "MODIS"
#' - "VIIRS_S_NPP"
#'
#' The following argument should be specified by users:
#'
#' \describe{
#'   \item{instrument}{A character vector specifying
#'   the data collection instrument.}
#'   }
#'
#' @name nasa_firms
#' @docType data
#' @keywords resource
#' @format Active fire polygon available for years 2000 to 2021 (MODIS)
#' and 2012-2021 (VIIRS)
#' @references NRT VIIRS 375 m Active Fire product VNP14IMGT distributed
#' from NASA FIRMS. Available on-line https://earthdata.nasa.gov/firms.
#' doi:10.5067/FIRMS/VIIRS/VNP14IMGT_NRT.002.
#' @source \url{https://firms.modaps.eosdis.nasa.gov/download/}
NULL


#' Downloads FIRMS active fire Polygon
#'
#' @param x An sf object returned by init_portfolio
#' @param instrument A character vector specifying the
#'   data collection instrument.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @keywords internal
#' @noRd

.get_nasa_firms <- function(x,
                            instrument = "VIIRS_S_NPP",
                            rundir = tempdir(),
                            verbose = TRUE) {
  target_years <- attributes(x)$years
  if (!instrument %in% c("MODIS", "VIIRS_S_NPP")) {
    stop(
      paste("The selected instrument", instrument, "is not available. Please choose one of: MODIS or VIIRS_S_NPP")
      )
  }

  if (instrument == "VIIRS_S_NPP") {
    available_years <- c(2012:2021)
    target_years <- .check_available_years(
      target_years, available_years, "active_fire"
    )
  } else {
    available_years <- c(2000:2021)
    target_years <- .check_available_years(
      target_years, available_years, "active_fire"
    )
  }

  # get wkt from the portfolio object
  wkt <- st_as_text(st_geometry(x))
  # get country name/s of the portfolio
  country <- .get_country_name(wkt, rundir)

  urls <- unlist(sapply(target_years, function(year) .get_firms_url(year, country, instrument)))
  filenames <- file.path(
    rundir,
    basename(paste0(target_years, "_", country, ".csv"))
  )
  if (attr(x, "testing")) {
    return(basename(filenames))
  }

  if (any(file.exists(filenames))) {
    message("Skipping existing files in output directory.")
  }
  # start download in a temporal directory within tmpdir
  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(urls, filenames, verbose, aria_bin = aria_bin)

  # convert csv to sf object and save as geopackage
  fire <- .convert_csv_to_gpkg(rundir)
  # write fire as geopackage
  write_sf(fire, file.path(rundir, "active_fire.gpkg"))
  # remove the csv file/s
  unlink(file.path(rundir, "*csv"))
  # return paths to the gpkg
  list.files(rundir, full.names = T, pattern = ".gpkg")
}

#' Helper function to get country name from the portfolio object
#'
#' @param wkt A well-known text representation of geometry
#' @param rundir A directory where intermediate files are written to.
#'
#' @return A character vector indicating country name
#' @keywords internal
#' @noRd
.get_country_name <- function(wkt,
                              rundir) {

  # get url of the gadm world boundaries polygon
  url <- paste("https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip")
  # start download in a temporal directory within rundir
  downloads <- tryCatch(
    {
      download.file(url,
        file.path(rundir, basename(paste0("World.zip"))),
        quiet = TRUE
      )
    },
    error = function(e) e,
    warning = function(e) e
  )
  if (inherits(downloads, "error")) stop(downloads)
  # unzip
  .unzip_and_remove(file.path(rundir, "World.zip"), rundir)
  # load world shapefile only for the extent of portfolio object
  gpkg <- read_sf(file.path(
    rundir,
    "gadm_410.gpkg"
  ),
  wkt_filter = wkt
  )
  # get ISO3 code of the country/ies
  iso_gadm <- data.frame(ISO3 = unique(gpkg$GID_0))
  # load ISO3 - country text file from the package
  iso_txt_file <- system.file("extdata", "countriesISO.csv",
    package = "mapme.biodiversity"
  )
  iso_txt <- read.csv(iso_txt_file)
  # merge
  iso_merged <- merge(iso_gadm,
    iso_txt,
    by = "ISO3"
  )
  # remove the world gpkg file
  unlink(file.path(rundir, "gadm_410.gpkg"))
  # get country name/s
  country_name <- as.character(iso_merged[[2]])
  country_name
}


#' A helper function to construct correct FIRMS layer urls
#'
#' @param target_year A numeric indicating the target year
#' @param country A character vector specifying
#'   the country name.
#' @param instrument A character vector specifying the
#'   data collection instrument.
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_firms_url <- function(target_year, country, instrument) {
  if (instrument == "VIIRS_S_NPP") {
    available_years <- c(2012:2021)
    if (target_year %in% available_years) {
      paste0("https://firms.modaps.eosdis.nasa.gov/data/country/viirs-snpp/", target_year, "/viirs-snpp_", target_year, "_", country, ".csv")
    } else {
      warning(
        sprintf(
          "FIRMS data from VIIRS S-NPP not available for target year %s", target_year
        )
      )
      NULL
    }
  } else {
    available_years <- c(2000:2021)
    if (target_year %in% available_years) {
      paste0("https://firms.modaps.eosdis.nasa.gov/data/country/modis/", target_year, "/modis_", target_year, "_", country, ".csv")
    } else {
      warning(
        sprintf(
          "FIRMS data from MODIS not available for target year %s", target_year
        )
      )
      NULL
    }
  }
}



#' Helper function to convert CSVs to geopackage
#'
#' @param rundir A directory where intermediate files are written to.
#'
#' @return A polygon object
#' @keywords internal
#' @noRd
#'
.convert_csv_to_gpkg <- function(rundir) {

  # bind all downloaded CSVs
  data_all <- list.files(rundir,
    pattern = "*.csv", full.names = TRUE
  ) %>%
    lapply(read.csv) %>%
    dplyr::bind_rows()
  # convert to sf object
  fire <- st_as_sf(
    x = data_all,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  return(fire)
}
