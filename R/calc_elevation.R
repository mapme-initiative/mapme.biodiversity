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
                            ...) {
  if (is.null(nasa_srtm)) {
    return(NA)
  }
  .select_engine(
    shp = shp,
    raster = nasa_srtm,
    stats = stats_elevation,
    engine = engine,
    name = "elevation",
    mode = "asset")
}

