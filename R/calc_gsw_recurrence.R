#' Calculate Global Surface Water (GSW) Recurrence
#'
#' Water Recurrence is a measurement of the degree of variability in the
#' presence of water from year to year. It describes the frequency with which
#' water returned to a particular location from one year to another, and is
#' expressed as a percentage. The raster files have integer cell values between
#' \code{[0, 100]}, where 100 represents that water reoccurs predictably every
#' year, whereas lower values indicate that water only occurs episodically.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats} parameter.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_recurrence]
#'
#' @name gsw_recurrence
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats Aggregation function with which the data are combined.
#' Default: "mean".
#' @keywords indicator
#' @returns A tibble with a column for the aggregated GSW recurrence indicator.
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
#'   get_resources(get_global_surface_water_recurrence()) %>%
#'   calc_indicators(
#'     calc_gsw_recurrence(engine = "extract", stats = "mean")
#'   ) %>%
#'   tidyr::unnest(gsw_recurrence)
#'
#' aoi
#' }
calc_gsw_recurrence <- function(engine = "extract", stats = "mean") {
  check_engine(engine)
  check_stats(stats)

  function(x,
           global_surface_water_recurrence = NULL,
           name = "gsw_recurrence",
           mode = "asset",
           rundir = mapme_options()[["tempdir"]],
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(global_surface_water_recurrence)) {
      return(NA)
    }

    global_surface_water_recurrence <- terra::clamp(
      global_surface_water_recurrence,
      lower = 0,
      upper = 100,
      values = FALSE
    )

    results <- select_engine(
      x = x,
      raster = global_surface_water_recurrence,
      stats = stats,
      engine = engine,
      name = "global_surface_water_recurrence",
      mode = "asset"
    )

    results
  }
}

register_indicator(
  name = "gsw_recurrence",
  resources = list(global_surface_water_recurrence = "raster")
)
