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
#' \dontshow{
#' mapme.biodiversity:::.copy_resource_dir(file.path(tempdir(), "mapme-data"))
#' }
#' \dontrun{
#' library(sf)
#' library(mapme.biodiversity)
#'
#' outdir <- file.path(tempdir(), "mapme-data")
#' dir.create(outdir, showWarnings = FALSE)
#'
#'
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2018,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("worldclim_precipitation") %>%
#'   calc_indicators("precipitation_wc",
#'     stats_worldclim = c("mean", "median"),
#'     engine = "extract"
#'   ) %>%
#'   tidyr::unnest(precipitation_wc)
#'
#' aoi
#' }
NULL

#' Calculate worldclim precipitation statistics
#'
#' Considering the 1km precipitation raster datasets from worldclim users
#' can specify which statistics among min, max, sum, mean, median, variance or
#' standard deviation to compute. Also, users can specify the functions i.e. zonal
#' from package terra, extract from package terra, or exactextract from exactextractr
#' as desired.
#'
#' @param x A single polygon for which to calculate the precipitation statistic
#' @param worldclim_precipitation precipitation raster from which to compute statistics
#' @param stats_worldclim Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_precipitation_wc <- function(x,
                                   worldclim_precipitation,
                                   engine = "extract",
                                   stats_worldclim = "mean",
                                   ...) {
  results <- .calc_worldclim(
    x = x,
    worldclim = worldclim_precipitation,
    engine = engine,
    stats_worldclim = stats_worldclim
  )
  results
}

register_indicator(
  name = "precipitation_wc",
  resources = list(worldclim_precipitation = "raster"),
  fun = .calc_precipitation_wc,
  arguments = list(
    engine = "extract",
    stats_worldclim = "mean"
  ),
  processing_mode = "asset"
)
