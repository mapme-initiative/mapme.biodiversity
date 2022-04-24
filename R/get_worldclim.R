#' WorldClim climatic variables (min temperature, max temperature, mean precipitation)
#'
#' This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
#' spatial resolution climate surfaces for global land areas". This resource
#' represents multiple climatic variables from which we will be requiring minimum
#' temperature, maximum temperature, and mean precipitation layers. The layers are
#' available to download for the period 2000 - 2018 on monthly basis from WorldClim.
#'
#' Enlisted different resources can be reqested with their dedicated functions:
#' \describe{
#'   \item{tmin}{Encoded as (°C), representing the minimum temperature per output grid cell.}
#'   \item{tmax}{Encoded as (°C), representing the maximum temperature per output grid cell.}
#'   \item{prec}{Encoded as (mm), representing the mean precipitation per output grid cell.}
#' }
#'
#' @name worldclim
#' @docType data
#' @keywords resource
#' @format Global raster layers available for years 2000 to 2018.
#' @source \url{https://www.worldclim.org/data/index.html}
NULL


#' Downloads WorldClim Minimum Temperature layer
#'
#' This resource represents the minimum temperature, layers available to
#' download for the period 2000 - 2018 on monthly basis from WorldClim. Encoded
#' as (°C), representing the minimum temperature per output grid cell.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name WorldClim_Minimum_Temperature
#' @keywords internal
#' @noRd

.get_min_temperature <- function(x,
                                 rundir = tempdir(),
                                 verbose = TRUE) {
  .get_climatic_variables(x = x, layer = "tmin", rundir, verbose)
}




#' Downloads WorldClim Maximum Temperature layer
#'
#' This resource represents the maximum temperature, layers available to
#' download for the period 2000 - 2018 on monthly basis from WorldClim. Encoded
#' as (°C), representing the maximum temperature per output grid cell.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name WorldClim_Maximum_Temperature
#' @keywords internal
#' @noRd

.get_max_temperature <- function(x,
                                 rundir = tempdir(),
                                 verbose = TRUE) {
  .get_climatic_variables(x = x, layer = "tmax", rundir, verbose)
}




#' Downloads WorldClim Mean Precipitation layer
#'
#' This resource represents the average precipitation, layers available to
#' download for the period 2000 - 2018 on monthly basis from WorldClim. Encoded
#' as (mm), representing the mean precipitation per output grid cell.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name WorldClim_Mean_Precipitation
#' @keywords internal
#' @noRd

.get_precipitation <- function(x,
                               rundir = tempdir(),
                               verbose = TRUE) {
  .get_climatic_variables(x = x, layer = "prec", rundir, verbose)
}


#' Helper function to download climate variable layers
#'
#' @param layer A character indicating the target variable name
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
.get_climatic_variables <- function(x,
                                    layer,
                                    rundir = tempdir(),
                                    verbose = TRUE) {
  if (missing(layer)) {
    stop(paste("No target layer has been specified. ",
      "Please select one of 'tmin', 'tmax', 'prec'.",
      sep = ""
    ))
  }

  target_years <- attributes(x)$years
  available_years <- 2000:2018
  target_years <- .check_available_years(
    target_years, available_years, layer
  )

  all_urls <- unlist(sapply(target_years, function(year) {
    .get_climate_url(layer, year)
  }))
  urls <- unique(all_urls)
  filenames <- file.path(rundir, basename(urls))
  # start download in a temporal directory within tmpdir
  # TODO: Parallel downloads
  aria_bin <- attributes(x)$aria_bin
  if (!is.null(attr(x, "testing"))) {
    return(filenames)
  }

  .download_or_skip(urls, filenames, verbose, aria_bin = aria_bin, check_existence = FALSE)

  # unzip the downloaded file
  sapply(filenames, function(zip) {
    .unzip_and_remove(zip, rundir, remove = FALSE)
  })

  # remove all except desired layers
  nontarget_years <- available_years[!available_years %in% target_years]

  for (i in seq_along(nontarget_years)) {
    unlink(file.path(
      rundir,
      paste0(
        "wc2.1_2.5m_", layer, "_",
        nontarget_years[i], "*.tif"
      )
    ),
    recursive = T, force = T
    )
  }

  # return paths to the raster
  list.files(rundir, full.names = T, pattern = ".tif")
}


#' Helper function to construct climate variable layers urls
#'
#' @param layer A character indicating the target variable name
#' @param year A numeric indicating the target year
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
.get_climate_url <- function(layer, year) {
  if (year %in% c(2000:2009)) {
    paste0(
      "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_",
      layer, "_2000-2009.zip"
    )
  } else if (year %in% c(2010:2018)) {
    paste0(
      "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_",
      layer, "_2010-2018.zip"
    )
  } else {
    warning(sprintf("Climate raster not available for target year %s", year))
    NULL
  }
}
