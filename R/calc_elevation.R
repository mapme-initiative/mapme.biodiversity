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
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2000:2020,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("nasa_srtm") %>%
#'   calc_indicators("elevation",
#'     stats_elevation = c("mean", "median", "sd", "var"), engine = "extract"
#'   ) %>%
#'   tidyr::unnest(elevation)
#'
#' aoi
#' }
NULL

#' Calculate elevation statistics based on SRTM data sets
#'
#' Considering the 30m resolution SRTM raster datasets users can specify which
#' statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param x A single polygon for which to calculate the elevation statistic
#' @param nasa_srtm The elevation raster resource from SRTM
#' @param stats Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character "mean", "median" or "sd".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_elevation <- function(x,
                            nasa_srtm,
                            engine = "extract",
                            stats_elevation = "mean",
                            verbose,
                            ...) {
  if (is.null(nasa_srtm)) {
    return(NA)
  }
  .select_engine(
    x = x,
    raster = nasa_srtm,
    stats = stats_elevation,
    engine = engine,
    name = "elevation",
    mode = "asset"
  )
}

register_indicator(
  name = "elevation",
  resources = list(nasa_srtm = "raster"),
  fun = .calc_elevation,
  arguments = list(
    engine = "extract",
    stats_elevation = "mean"
  ),
  processing_mode = "asset"
)
