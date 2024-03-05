#' Calculate Global Surface Water (GSW) Seasonality
#'
#' GSW seasonality describes the intra-annual distribution of surface water for
#' each pixel. The raster files have integer cell values between \code{[0, 12]},
#' indicating how many months per year the pixel was classified as water.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats} parameter.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_seasonality]
#'
#' @name gsw_seasonality
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats Aggregation function with which the data are combined.
#' Default: "mean".
#' @keywords indicator
#' @returns A tibble with a column for the aggregated GSW seasonality indicator.
#' @include register.R
#' @export
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
#' mapme_options(
#'   outdir = outdir,
#'   tmpdir = tempdir(),
#'   verbose = FALSE
#' )
#'
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   get_resources(get_global_surface_water_seasonality()) %>%
#'   calc_indicators(
#'     calc_gsw_seasonality(engine = "extract", stats = "mean")
#'   ) %>%
#'   tidyr::unnest(gsw_seasonality)
#'
#' aoi
#' }
calc_gsw_seasonality <- function(engine = "extract", stats = "mean") {
  check_engine(engine)
  check_stats(stats)

  function(x,
           global_surface_water_seasonality = NULL,
           name = "gsw_seasonality",
           mode = "asset",
           rundir = mapme_options()[["tempdir"]],
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(global_surface_water_seasonality)) {
      return(NA)
    }

    global_surface_water_seasonality <- terra::clamp(
      global_surface_water_seasonality,
      lower = 0,
      upper = 12,
      values = FALSE
    )

    results <- select_engine(
      x = x,
      raster = global_surface_water_seasonality,
      stats = stats,
      engine = engine,
      name = "global_surface_water_seasonality",
      mode = "asset"
    )

    results
  }
}

register_indicator(
  name = "gsw_seasonality",
  resources = list(global_surface_water_seasonality = "raster")
)
