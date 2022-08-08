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
#' - "VIIRS"
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
                            instrument = "VIIRS",
                            rundir = tempdir(),
                            verbose = TRUE) {
  target_years <- attributes(x)$years
  if (!instrument %in% c("MODIS", "VIIRS")) {
    stop(
      paste("The selected instrument", instrument, "is not available. Please choose one of: MODIS or VIIRS")
    )
  }

  if (instrument == "VIIRS") {
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

  urls <- unlist(sapply(target_years, function(year) .get_firms_url(year, instrument)))
  filenames <- file.path(
    rundir,
    basename(paste0(instrument, "_", target_years, ".zip"))
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

  # unzip zip files
  sapply(filenames, function(zip) .unzip_firms(zip, rundir, instrument))
  # remove unnecessary files
  d_files <- list.files(rundir, full.names = T)
  unlink(grep("_20*", d_files, value = T, invert = T),
    recursive = T, force = T
  )
  # return paths to the gpkg
  list.files(rundir, full.names = T, pattern = ".gpkg")
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
.get_firms_url <- function(target_year, instrument) {
  if (instrument == "VIIRS") {
    available_years <- c(2012:2021)
    if (target_year %in% available_years) {
      paste0("https://firms.modaps.eosdis.nasa.gov/data/country/zips/viirs-snpp_", target_year, "_all_countries.zip")
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
      paste0("https://firms.modaps.eosdis.nasa.gov/data/country/zips/modis_", target_year, "_all_countries.zip")
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


#' A helper function to unzip firms global zip files
#'
#' @param zip A character vector with potentially multiple zip files
#' @param rundir The directory to where the files are unzipped
#' @param instrument A character vector specifying the
#'   data collection instrument.
#' @importFrom base tolower
#' @return Nothing.
#' @keywords internal
#' @noRd
.unzip_firms <- function(zip, rundir, instrument) {
  bn <- basename(zip)
  year <- gsub(".*?([0-9]+).*", "\\1", bn)

  gpkg <- file.path(rundir, paste0("active_fire_", tolower(instrument), "_", year, ".gpkg"))
  if (file.exists(gpkg)) {
    return(gpkg)
  }

  utils::unzip(
    zipfile = file.path(
      rundir,
      basename(paste0(instrument, "_", year, ".zip"))
    ),
    exdir = rundir
  )

  # bind all the CSVs and convert to gpkg
  .convert_csv_to_gpkg(
    rundir = rundir,
    instrument = instrument,
    year = year
  )
}


#' Helper function to convert CSVs to geopackage
#'
#' @param rundir A directory where intermediate files are written to.
#' @param instrument A character vector specifying the
#'   data collection instrument.
#'
#' @importFrom utils read.csv
#' @importFrom plyr rbind.fill
#' @return Nothing.
#' @keywords internal
#' @noRd
#'
.convert_csv_to_gpkg <- function(rundir, instrument, year) {
  if (instrument == "VIIRS") {

    # bind all the CSVs
    data_all <- suppressWarnings(list.files(paste0(rundir, "/viirs-snpp/", year, "/"),
      pattern = "*.csv", full.names = TRUE
    ) %>%
      lapply(read.csv) %>%
      plyr::rbind.fill())

  } else {

    # bind all the CSVs
    data_all <- suppressWarnings(list.files(paste0(rundir, "/modis/", year, "/"),
      pattern = "*.csv", full.names = TRUE
    ) %>%
      lapply(read.csv) %>%
      plyr::rbind.fill())
  }

  # convert to sf object
  fire <- st_as_sf(
    x = data_all,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # write fire as geopackage
  write_sf(fire, file.path(
    rundir,
    paste0("active_fire_", tolower(instrument), "_", year, ".gpkg")
  ))
}
