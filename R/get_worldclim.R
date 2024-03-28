#' Downloads WorldClim Minimum Temperature layer
#'
#' This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
#' spatial resolution climate surfaces for global land areas" and
#' represents multiple climatic variables from which we will be requiring minimum
#' temperature, maximum temperature, and mean precipitation layers. The layers are
#' available to download for the period 2000 - 2018 on monthly basis from WorldClim.
#'
#' This resource represents the minimum temperature, layers available to
#' download for the period 2000 - 2018 on monthly basis from WorldClim. Encoded
#' as (°C), representing the minimum temperature per output grid cell.
#'
#' @name worldclim_min_temperature
#' @param years A numeric vector indicating for which years to make the
#'   resource available.
#' @keywords resource
#' @returns A function that returns a character of file paths.
#' @source \url{https://www.worldclim.org/data/index.html}
#' @importFrom utils unzip
#' @include register.R
#' @export
get_worldclim_min_temperature <- function(years = 2000:2018) {
  years <- check_available_years(years, 2000:2018, "tmax")

  function(x,
           name = "worldclim_min_temperature",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    .get_climatic_variables(x = x, years = years, layer = "tmin", outdir, verbose, testing)
  }
}

#' Downloads WorldClim Maximum Temperature layer
#'
#' This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
#' spatial resolution climate surfaces for global land areas" and
#' represents multiple climatic variables from which we will be requiring minimum
#' temperature, maximum temperature, and mean precipitation layers. The layers are
#' available to download for the period 2000 - 2018 on monthly basis from WorldClim.
#'
#' This resource represents the maximum temperature, layers available to
#' download for the period 2000 - 2018 on monthly basis from WorldClim. Encoded
#' as (°C), representing the maximum temperature per output grid cell.
#'
#' @name worldclim_max_temperature
#' @param years A numeric vector indicating for which years to make the
#'   resource available.
#' @keywords resource
#' @returns  A character of file paths.
#' @source \url{https://www.worldclim.org/data/index.html}
#' @importFrom utils unzip
#' @include register.R
#' @export
get_worldclim_max_temperature <- function(years = 2000:2018) {
  years <- check_available_years(years, 2000:2018, "tmax")

  function(x,
           name = "worldclim_max_temperature",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    .get_climatic_variables(x = x, years = years, layer = "tmax", outdir, verbose, testing)
  }
}

#' Downloads WorldClim Mean Precipitation layer
#'
#' This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
#' spatial resolution climate surfaces for global land areas" and
#' represents multiple climatic variables from which we will be requiring minimum
#' temperature, maximum temperature, and mean precipitation layers. The layers are
#' available to download for the period 2000 - 2018 on monthly basis from WorldClim.
#'
#' This resource represents the average precipitation, layers available to
#' download for the period 2000 - 2018 on monthly basis from WorldClim. Encoded
#' as (mm), representing the mean precipitation per output grid cell.
#'
#' @name worldclim_precipitation
#' @param years A numeric vector indicating for which years to make the
#'   resource available.
#' @keywords resource
#' @returns  A character of file paths.
#' @source \url{https://www.worldclim.org/data/index.html}
#' @importFrom utils unzip
#' @include register.R
#' @export
get_worldclim_precipitation <- function(years = 2000:2018) {
  years <- check_available_years(years, 2000:2018, "prec")

  function(x,
           name = "worldclim_precipitation",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    .get_climatic_variables(x = x, years = years, layer = "prec", outdir, verbose, testing)
  }
}


.get_climatic_variables <- function(x,
                                    years = 2000:2018,
                                    layer,
                                    dir = tempdir(),
                                    verbose = TRUE,
                                    testing = FALSE) {
  available_years <- 2000:2018
  if (missing(layer)) {
    stop(paste("No target layer has been specified. ",
      "Please select one of 'tmin', 'tmax', 'prec'.",
      sep = ""
    ))
  }

  all_urls <- unlist(sapply(years, function(year) {
    .get_climate_url(layer, year)
  }))
  urls <- unique(all_urls)
  filenames <- file.path(dir, basename(urls))
  if (testing) {
    return(basename(filenames))
  }
  download_or_skip(urls, filenames, check_existence = FALSE)

  # unzip the downloaded file
  sapply(filenames, function(zip) {
    unzip_and_remove(zip, dir, remove = FALSE)
  })

  # remove all except desired layers
  nontarget_years <- available_years[!available_years %in% years]

  for (i in seq_along(nontarget_years)) {
    unlink(
      file.path(
        dir,
        paste0(
          "wc2.1_2.5m_", layer, "_",
          nontarget_years[i], "*.tif"
        )
      ),
      recursive = T, force = T
    )
  }

  # return paths to the raster
  list.files(dir, full.names = T, pattern = ".tif")
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


register_resource(
  name = "worldclim_min_temperature",
  description = "WorldClim - Monthly minimum temperature 2000 - 2018",
  licence = "https://www.worldclim.org/about.html",
  source = "https://www.worldclim.org/data/index.html",
  type = "raster"
)

register_resource(
  name = "worldclim_max_temperature",
  description = "WorldClim - Monthly maximum temperature 2000 - 2018",
  licence = "https://www.worldclim.org/about.html",
  source = "https://www.worldclim.org/data/index.html",
  type = "raster"
)

register_resource(
  name = "worldclim_precipitation",
  description = "WorldClim - Monthly precipitation 2000 - 2018",
  licence = "https://www.worldclim.org/about.html",
  source = "https://www.worldclim.org/data/index.html",
  type = "raster"
)
