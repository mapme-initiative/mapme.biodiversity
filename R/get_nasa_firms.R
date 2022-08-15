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
      target_years, available_years, "nasa_firms"
    )
  } else {
    available_years <- c(2000:2021)
    target_years <- .check_available_years(
      target_years, available_years, "nasa_firms"
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
  list.files(rundir, full.names = T, pattern = ".gpkg$")
}


#' A helper function to construct correct FIRMS layer urls
#'
#' @param target_year A numeric indicating the target year
#' @param instrument A character vector specifying the
#'   data collection instrument.
#'
#' @return A character vector with the URL to the zip file of the respective year.
#' @keywords internal
#' @noRd
.get_firms_url <- function(target_year, instrument) {
  if (instrument == "VIIRS") {
    paste0("https://firms.modaps.eosdis.nasa.gov/data/country/zips/viirs-snpp_", target_year, "_all_countries.zip")
  } else {
    paste0("https://firms.modaps.eosdis.nasa.gov/data/country/zips/modis_", target_year, "_all_countries.zip")
  }
}


#' A helper function to unzip firms global zip files
#'
#' @param zip A character vector with potentially multiple zip files
#' @param rundir The directory to where the files are unzipped
#' @param instrument A character vector specifying the
#'   data collection instrument.
#' @return Nothing, its called for the side effect of unzipping.
#' @keywords internal
#' @noRd
.unzip_firms <- function(zip, rundir, instrument) {
  bn <- basename(zip)
  year <- gsub(".*?([0-9]+).*", "\\1", bn)

  gpkg <- file.path(rundir, paste0("active_fire_", tolower(instrument), "_", year, ".gpkg"))

  if (file.exists(gpkg)) {
    return()
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
    gpkg = gpkg,
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
#' @param year An integer indicating the year of observation
#' @param gpkg A character vector indicating the filename for the geopackage
#'
#' @importFrom utils read.csv
#' @importFrom purrr walk
#' @return Nothing, its called for the side effect to create a gpkg.
#' @keywords internal
#' @noRd
#'
.convert_csv_to_gpkg <- function(gpkg, rundir, instrument, year) {

  if (instrument == "VIIRS") {
    loc <- file.path(rundir, "viirs-snpp", year)
  } else {
    loc <- file.path(rundir, "modis", year)
  }

  csv_files <- list.files(loc, pattern = "*.csv", full.names = TRUE)

  walk(csv_files, function(file){
    data <- read.csv(file)
    data <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
    write_sf(data, dsn = gpkg, append = TRUE)
  })
}
