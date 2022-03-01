#' Downloads WorldClim Minimum Temperature layer
#'
#' This resource represents the minimum temperature, layers available to download for
#' the period 2000 - 2018 on monthly basis from WorldClim. Encoded as (°C), representing
#' the minimum temperature per output grid cell.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name WorldClim_Minimum_Temperature
#' @keywords internal
#'

.get_minTemperature <- function(x,
                                rundir = tempdir(),
                                verbose = TRUE) {
  .get_climaticVariables(x = x, layer = "tmin", rundir, verbose)
  list.files(rundir, full.names = T)
}




#' Downloads WorldClim Maximum Temperature layer
#'
#' This resource represents the maximum temperature, layers available to download for
#' the period 2000 - 2018 on monthly basis from WorldClim. Encoded as (°C), representing
#' the maximum temperature per output grid cell.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name WorldClim_Maximum_Temperature
#' @keywords internal
#'

.get_maxTemperature <- function(x,
                                rundir = tempdir(),
                                verbose = TRUE) {
  .get_climaticVariables(x = x, layer = "tmax", rundir, verbose)
  list.files(rundir, full.names = T)
}




#' Downloads WorldClim Mean Precipitation layer
#'
#' This resource represents the average precipitation, layers available to download for
#' the period 2000 - 2018 on monthly basis from WorldClim. Encoded as (mm), representing
#' the mean precipitation per output grid cell.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name WorldClim_Mean_Precipitation
#' @keywords internal
#'

.get_precipitation <- function(x,
                               rundir = tempdir(),
                               verbose = TRUE) {
  .get_climaticVariables(x = x, layer = "prec", rundir, verbose)
  list.files(rundir, full.names = T)
}


.get_climaticVariables <- function(x,
                                   layer = c("tmin", "tmax", "prec"),
                                   rundir = tempdir(),
                                   verbose = TRUE) {
  target_years <- attributes(x)$years
  all_urls <- unlist(sapply(target_years, function(year) .getClimateURL(layer, year)))
  urls <- unique(all_urls)

  # start download in a temporal directory within tmpdir
  if (verbose) pb <- progress_bar$new(total = length(urls))
  for (url in urls) {
    tryCatch(
      {
        if (verbose) pb$tick(0)
        download.file(url, file.path(rundir, basename(paste0(layer, "_", gsub("\\D", "", basename(url)), ".zip"))), quiet = TRUE)
        if (verbose) pb$tick()
      },
      error = function(e) {
        message("reading URLs!")
      }
    )
  }

  all_zips <- list.files(rundir, full.names = T)
  sapply(all_zips, function(zip) .unzipClimate(zip, rundir))

  # remove all except desired layers
  all_files <- list.files(rundir, full.names = T)
  available_years <- 2000:2018
  nontarget_years <- available_years[!available_years %in% target_years]

  for (i in 1:length(nontarget_years)) {
    unlink(paste0(rundir, "/wc2.1_2.5m_", layer, "_", nontarget_years[i], "*.tif"), recursive = T, force = T)
  }

  # return paths to the raster
  list.files(rundir, full.names = T)
}


.unzipClimate <- function(zips, rundir) {
  unzip(
    zipfile = file.path(rundir, basename(zips)),
    exdir = rundir
  )
  unlink(paste0(rundir, "/", basename(zips)))
}


.getClimateURL <- function(layer, year) {
  if (year %in% c(2000:2009)) {
    url <- paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_", layer, "_2000-2009.zip")
    url
  } else if (year %in% c(2010:2018)) {
    url <- paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_", layer, "_2010-2018.zip")
    url
  } else {
    warning(sprintf("Climate raster not available for target year %s", year))
    NULL
  }
}

