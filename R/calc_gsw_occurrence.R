#' Calculate Global Surface Water (GSW) Occurrence
#'
#' GSW occurrence raw data comes in raster files with integer cell values
#' between \code{[0, 100]}. This value gives the percentage of the time that a
#' given pixel was classified as water during the entire observation period. So
#' a 0 denotes a pixel that was never classified as water, 100 denotes a pixel
#' with permanent water.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats_gsw} parameter.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_occurrence]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_gsw}{The aggregation function applied to the input raster
#'   values. Defaults to \code{mean}.}
#' }
#'
#' @name gsw_occurrence
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for the aggregated GSW occurrence indicator.
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
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2001,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("global_surface_water_occurrence") %>%
#'   calc_indicators("gsw_occurrence")
#'
#' aoi
#' }
NULL

#' Calculate Global Surface Water (GSW) Occurrence
#'
#' GSW occurrence raw data comes in raster files with integer cell values
#' between \code{[0, 100]}. This value gives the percentage of the time that a
#' given pixel was classified as water during the entire observation period. So
#' a 0 denotes a pixel that was never classified as water, 100 denotes a pixel
#' with permanent water.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats_gsw} parameter.
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_occurrence The GSW Occurrence data source.
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats_gsw Aggregation function with which the data are combined.
#' Default: "mean".
#' @return A tibble containing the aggregated occurrence indicator. The column
#' name is a concatenation of "global_surface_water_occurrence_" +
#' \code{stats_gsw}.
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_occurrence <- function(x,
                                 global_surface_water_occurrence,
                                 engine = "extract",
                                 stats_gsw = "mean",
                                 verbose = TRUE,
                                 ...) {
  if (is.null(global_surface_water_occurrence)) {
    return(NA)
  }

  global_surface_water_occurrence <- terra::clamp(
    global_surface_water_occurrence,
    lower = 0,
    upper = 100,
    values = FALSE
  )

  results <- .select_engine(
    x = x,
    raster = global_surface_water_occurrence,
    stats = stats_gsw,
    engine = engine,
    name = "global_surface_water_occurrence",
    mode = "asset"
  )

  results
}

register_indicator(
  name = "gsw_occurrence",
  resources = list(global_surface_water_occurrence = "raster"),
  fun = .calc_gsw_occurrence,
  arguments = list(
    engine = "extract",
    stats_gsw = "mean"
  ),
  processing_mode = "asset"
)
