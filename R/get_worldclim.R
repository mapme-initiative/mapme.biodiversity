#' WorldClim climatic variables (min temperature, max temperature, mean precipitation)
#'
#' This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
#' spatial resolution climate surfaces for global land areas". This resource
#' represents multiple climatic variables from which we will be requiring minimum
#' temperature, maximum temperature, and mean precipitation layers. The layers are
#' available to download for the period 2000 - 2018 on monthly basis from WorldClim.
#'
#' Enlisted different resources can be requested with their dedicated functions:
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
#' @include register.R
#' @noRd
.get_worldclim_min_temperature <- function(x,
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
#' @include register.R
#' @noRd
.get_worldclim_max_temperature <- function(x,
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
#' @include register.R
#' @noRd
.get_worldclim_precipitation <- function(x,
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
  target_years <- check_available_years(
    target_years, available_years, layer
  )

  urls <- purrr::map(target_years, function(year) .get_climate_url(layer, year))
  urls <- unlist(urls)
  fps <- purrr::map(urls, function(url){
    st_bbox(c(xmin=-180., ymin=-90., xmax=180., ymax=90.), crs = "EPSG:4326") %>%
      st_as_sfc() %>%
      st_as_sf() %>%
      dplyr::mutate(source = url)
  })
  fps <- st_as_sf(purrr::list_rbind(fps))
  make_footprints(fps, what = "raster")
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
    baseurl <- paste0(
      "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_",
      layer, "_2000-2009.zip"
    )
  } else if (year %in% c(2010:2018)) {
    baseurl <- paste0(
      "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_",
      layer, "_2010-2018.zip"
    )
  }
  months <- sprintf('%02d',1:12)
  dates <- sprintf("%s-%s", year, months)
  filenames <- sprintf("wc2.1_2.5m_%s_%s.tif", layer, dates)
  file.path("/vsizip//vsicurl", baseurl, filenames)
}


register_resource(
  name = "worldclim_min_temperature",
  type = "raster",
  source = "https://www.worldclim.org/data/index.html",
  fun = .get_worldclim_min_temperature,
  arguments <- list()
)

register_resource(
  name = "worldclim_max_temperature",
  type = "raster",
  source = "https://www.worldclim.org/data/index.html",
  fun = .get_worldclim_max_temperature,
  arguments <- list()
)

register_resource(
  name = "worldclim_precipitation",
  type = "raster",
  source = "https://www.worldclim.org/data/index.html",
  fun = .get_worldclim_precipitation,
  arguments <- list()
)
