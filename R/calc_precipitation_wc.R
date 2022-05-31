#' Calculate precipitation statistics
#'
#' This function allows to efficiently calculate precipitation statistics
#' from Worldclim for polygons. For each polygon, the desired statistic/s (min,
#' max, sum, mean, median, sd or var) is/are returned.
#' The required resources for this indicator are:
#'  - precipitation layer from [worldclim]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_worldclim}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name precipitation_wc
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for precipitation statistics (in mm)
#' @examples
#' library(sf)
#' library(mapme.biodiversity)
#'
#' temp_loc <- file.path(tempdir(), "mapme.biodiversity")
#' if (!file.exists(temp_loc)) {
#'   dir.create(temp_loc)
#'   resource_dir <- system.file("res", package = "mapme.biodiversity")
#'   file.copy(resource_dir, temp_loc, recursive = TRUE)
#' }
#'
#'
#' (try(aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2018,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("worldclim_precipitation") %>%
#'   calc_indicators("precipitation_wc",
#'     stats_worldclim = c("mean", "median"),
#'     engine = "extract"
#'   ) %>%
#'   tidyr::unnest(precipitation_wc)))
NULL

#' Calculate worldclim precipitation statistics
#'
#' Considering the 1km precipitation raster datasets from worldclim users
#' can specify which statistics among min, max, sum, mean, median, variance or
#' standard deviation to compute. Also, users can specify the functions i.e. zonal
#' from package terra, extract from package terra, or exactextract from exactextractr
#' as desired.
#'
#' @param shp A single polygon for which to calculate the precipitation statistic
#' @param worldclim_precipitation precipitation raster from which to compute statistics
#' @param stats_worldclim Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
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

.calc_precipitation_wc <- function(shp,
                                   worldclim_precipitation,
                                   engine = "extract",
                                   stats_worldclim = "mean",
                                   rundir = tempdir(),
                                   verbose = TRUE,
                                   todisk = FALSE,
                                   ...) {
  results <- .calc_worldclim(
    shp = shp,
    worldclim = worldclim_precipitation,
    engine = engine,
    stats_worldclim = stats_worldclim,
    rundir = rundir,
    verbose = verbose,
    todisk = todisk
  )
  results
}
