#' Downloads WorldClim Minimum Temperature layer
#'
#' This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
#' spatial resolution climate surfaces for global land areas" and
#' represents multiple climatic variables from which we will be requiring minimum
#' temperature, maximum temperature, and mean precipitation layers. The layers are
#' available to download for the period 1960 - 2024 on monthly basis from WorldClim.
#'
#' This resource represents the minimum temperature, layers available to
#' download for the period 1960 - 2024 on monthly basis from WorldClim. Encoded
#' as (°C), representing the minimum temperature per output grid cell.
#'
#' @name worldclim_min_temperature
#' @param years A numeric vector indicating for which years to make the
#'   resource available. Defaults to 2000:2024.
#' @param resolution A character vector indicating the desired resolution:
#' "2.5m", "5m", or "10m".
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @source \url{https://www.worldclim.org/data/index.html}
#' @importFrom utils unzip
#' @include register.R
#' @export
get_worldclim_min_temperature <- function(years = 2000:2024,
                                          resolution = c("2.5m", "5m", "10m")) {
  years <- check_available_years(years, 1960:2024, "tmin")
  resolution <- match.arg(resolution)

  function(x,
           name = "worldclim_min_temperature",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .get_climatic_variables(x = x, years = years, var = "tmin", res = resolution, verbose)
  }
}

#' Downloads WorldClim Maximum Temperature layer
#'
#' This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
#' spatial resolution climate surfaces for global land areas" and
#' represents multiple climatic variables from which we will be requiring minimum
#' temperature, maximum temperature, and mean precipitation layers. The layers are
#' available to download for the period 1960 - 2024 on monthly basis from WorldClim.
#'
#' This resource represents the maximum temperature, layers available to
#' download for the period 1960 - 2024 on monthly basis from WorldClim. Encoded
#' as (°C), representing the maximum temperature per output grid cell.
#'
#' @name worldclim_max_temperature
#' @param years A numeric vector indicating for which years to make the
#'   resource available. Defaults to 2000:2024.
#' @param resolution A character vector indicating the desired resolution:
#' "2.5m", "5m", or "10m".
#' @keywords resource
#' @returns  A function that returns an `sf` footprint object.
#' @source \url{https://www.worldclim.org/data/index.html}
#' @importFrom utils unzip
#' @include register.R
#' @export
get_worldclim_max_temperature <- function(years = 2000:2024,
                                          resolution = c("2.5m", "5m", "10m")) {
  years <- check_available_years(years, 1960:2024, "tmax")
  resolution <- match.arg(resolution)

  function(x,
           name = "worldclim_max_temperature",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .get_climatic_variables(x = x, years = years, var = "tmax", res = resolution, verbose)
  }
}

#' Downloads WorldClim Mean Precipitation layer
#'
#' This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
#' spatial resolution climate surfaces for global land areas" and
#' represents multiple climatic variables from which we will be requiring minimum
#' temperature, maximum temperature, and mean precipitation layers. The layers are
#' available to download for the period 1960 - 2024 on monthly basis from WorldClim.
#'
#' This resource represents the average precipitation, layers available to
#' download for the period 1960 - 2024 on monthly basis from WorldClim. Encoded
#' as (mm), representing the mean precipitation per output grid cell.
#'
#' @name worldclim_precipitation
#' @param years A numeric vector indicating for which years to make the
#'   resource available.
#' @param resolution A character vector indicating the desired resolution.
#' @keywords resource
#' @returns  A function that returns an `sf` footprint object.
#' @source \url{https://www.worldclim.org/data/index.html}
#' @importFrom utils unzip
#' @include register.R
#' @export
get_worldclim_precipitation <- function(years = 1960:2024,
                                        resolution = c("2.5m", "5m", "10m")) {
  years <- check_available_years(years, 1960:2024, "prec")
  resolution <- match.arg(resolution)

  function(x,
           name = "worldclim_precipitation",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .get_climatic_variables(x = x, years = years, var = "prec", res = resolution, verbose)
  }
}


.get_climatic_variables <- function(x,
                                    years = 2000:2024,
                                    var,
                                    res,
                                    verbose = TRUE) {
  urls <- purrr::map(years, function(year) .get_worldclim_url(res, year, var))
  urls <- unlist(urls)
  bbox <- c(xmin = -180., ymin = -90., xmax = 180., ymax = 90.)
  tiles <- st_as_sfc(st_bbox(bbox, crs = "EPSG:4326"))
  tiles <- st_as_sf(rep(tiles, length(urls)))
  tiles[["source"]] <- urls
  make_footprints(tiles, what = "raster", co = c("-co", "COMPRESS=LZW"))
}


#' Helper function to construct climate variable layers urls
#'
#' @param layer A character indicating the target variable name
#' @param year A numeric indicating the target year
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
.get_worldclim_url <- function(res, year, var) {
  stopifnot(length(res) == 1 && res %in% c("2.5m", "5m", "10m"))
  stopifnot(length(year) == 1 && year %in% 1960:2024)
  stopifnot(length(var) == 1 && var %in% c("prec", "tmin", "tmax"))

  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.09/"
  zipfile <- "wc2.1_cruts4.09_%s_%s_%s-%s.zip"

  start <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
  end <- c(1969, 1979, 1989, 1999, 2009, 2019, 2024)
  index <- which(start <= year & end >= year)

  zipfile <- sprintf(zipfile, res, var, start[index], end[index])

  url <- file.path(base_url, zipfile)

  months <- sprintf("%02d", 1:12)
  dates <- sprintf("%s-%s", year, months)
  tifs <- sprintf("wc2.1_cruts4.09_%s_%s_%s.tif", res, var, dates)
  file.path("/vsizip//vsicurl", url, tifs)
}


register_resource(
  name = "worldclim_min_temperature",
  description = "WorldClim - Monthly minimum temperature 1960 - 2024",
  licence = "https://www.worldclim.org/about.html",
  source = "https://www.worldclim.org/data/index.html",
  type = "raster"
)

register_resource(
  name = "worldclim_max_temperature",
  description = "WorldClim - Monthly maximum temperature 1960 - 2024",
  licence = "https://www.worldclim.org/about.html",
  source = "https://www.worldclim.org/data/index.html",
  type = "raster"
)

register_resource(
  name = "worldclim_precipitation",
  description = "WorldClim - Monthly precipitation 1960 - 2024",
  licence = "https://www.worldclim.org/about.html",
  source = "https://www.worldclim.org/data/index.html",
  type = "raster"
)
