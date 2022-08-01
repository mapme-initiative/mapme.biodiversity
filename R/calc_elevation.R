#' Calculate elevation statistics
#'
#' This function allows to efficiently calculate elevation statistics for
#' polygons. For each polygon, the desired statistic/s (mean, median or sd)
#' is/are returned.
#' The required resources for this indicator are:
#'  - [nasa_srtm]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_elevation}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character.  Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name elevation
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for elevation statistics (in meters)
#' @examples
#' if (Sys.getenv("NOT_CRAN") == "true") {
#'   library(sf)
#'   library(mapme.biodiversity)
#'
#'   temp_loc <- file.path(tempdir(), "mapme.biodiversity")
#'   if (!file.exists(temp_loc)) {
#'     dir.create(temp_loc)
#'     resource_dir <- system.file("res", package = "mapme.biodiversity")
#'     file.copy(resource_dir, temp_loc, recursive = TRUE)
#'   }
#'
#'   (try(aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'     package = "mapme.biodiversity"
#'   ) %>%
#'     read_sf() %>%
#'     init_portfolio(
#'       years = 2000:2020,
#'       outdir = file.path(temp_loc, "res"),
#'       tmpdir = tempdir(),
#'       add_resources = FALSE,
#'       cores = 1,
#'       verbose = FALSE
#'     ) %>%
#'     get_resources("nasa_srtm") %>%
#'     calc_indicators("elevation",
#'       stats_elevation = c("mean", "median", "sd", "var"), engine = "extract"
#'     ) %>%
#'     tidyr::unnest(elevation)))
#' }
NULL

#' Calculate elevation statistics based on SRTM data sets
#'
#' Considering the 30m resolution SRTM raster datasets users can specify which
#' statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param shp A single polygon for which to calculate the elevation statistic
#' @param nasa_srtm The elevation raster resource from SRTM
#' @param stats Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character "mean", "median" or "sd".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @noRd

.calc_elevation <- function(shp,
                            nasa_srtm,
                            engine = "zonal",
                            stats_elevation = "mean",
                            rundir = tempdir(),
                            verbose,
                            todisk = FALSE,
                            ...) {
  if (is.null(nasa_srtm)) {
    return(NA)
  }
  # check if intermediate raster should be written to disk
  if (ncell(nasa_srtm) > 1024 * 1024) todisk <- TRUE
  # check if input engine is correctly specified
  available_engines <- c("zonal", "extract", "exactextract")
  .check_engine(available_engines, engine)
  # check if only supoorted stats have been specified
  available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
  .check_stats(available_stats, stats_elevation)

  if (engine == "extract") {
    tibble_zstats <- .comp_dem_extract(
      elevation = nasa_srtm,
      shp = shp,
      stats = stats_elevation
    )
    return(tibble_zstats)
  } else if (engine == "exactextract") {
    tibble_zstats <- .comp_dem_exact_extractr(
      elevation = nasa_srtm,
      shp = shp,
      stats = stats_elevation
    )
    return(tibble_zstats)
  } else {
    tibble_zstats <- .comp_dem_zonal(
      elevation = nasa_srtm,
      shp = shp,
      stats = stats_elevation,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  }
}

#' Helper function to compute statistics using routines from terra extract
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_dem_extract <- function(elevation = NULL,
                              shp = NULL,
                              stats = "mean",
                              ...) {
  shp_v <- vect(shp)
  zstats <- lapply(1:length(stats), function(i) {
    zstats <- terra::extract(elevation,
      shp_v,
      fun = stats[i],
      na.rm = T
    )
    tibble_zstats <- tibble(elev = zstats[, 2])
    names(tibble_zstats)[names(tibble_zstats) == "elev"] <-
      paste0("elevation_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}

#' Helper function to compute statistics using routines from terra zonal
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_dem_zonal <- function(elevation = NULL,
                            shp = NULL,
                            stats = "mean",
                            todisk = FALSE,
                            rundir = tempdir(),
                            ...) {
  shp_v <- vect(shp)
  rast_mask <- terra::mask(elevation,
    shp_v,
    filename =  ifelse(todisk, file.path(rundir, "elevation.tif"), ""),
    overwrite = TRUE
  )
  p_raster <- terra::rasterize(shp_v,
    rast_mask,
    field = 1:nrow(shp_v),
    filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    overwrite = TRUE
  )
  zstats <- lapply(1:length(stats), function(i) {
    zstats <- terra::zonal(rast_mask,
      p_raster,
      fun = stats[i],
      na.rm = T
    )
    tibble_zstats <- tibble(elev = zstats[, 2])
    names(tibble_zstats)[names(tibble_zstats) == "elev"] <-
      paste0("elevation_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}

#' Helper function to compute statistics using routines from exactextractr
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_dem_exact_extractr <- function(elevation = NULL,
                                     shp = NULL,
                                     stats = "mean",
                                     ...) {
  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }
  zstats <- lapply(1:length(stats), function(i) {
    if (stats[i] %in% c("sd", "var")) {
      zstats <- exactextractr::exact_extract(
        elevation,
        shp,
        fun = ifelse(stats[i] == "sd", "stdev", "variance")
      )
    } else {
      zstats <- exactextractr::exact_extract(
        elevation,
        shp,
        fun = stats[i]
      )
    }
    tibble_zstats <- tibble(elev = zstats)
    names(tibble_zstats)[names(tibble_zstats) == "elev"] <-
      paste0("elevation_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}
