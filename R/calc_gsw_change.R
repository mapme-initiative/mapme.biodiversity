#' Calculate Global Surface Water (GSW) Change
#'
#' The change in water occurrence intensity between the two periods is derived
#' from homologous pairs of months (i.e. same months containing valid
#' observations in both periods). The difference in the occurrence of surface
#' water was calculated for each homologous pair of months. The average of all
#' of these differences constitutes the Surface Water Occurrence change
#' intensity. The raster files have integer cell values between `[0, 200]`
#' where 0 represents surface water loss and 200 represents surface water gain.
#'
#' The pixel values are aggregated using method provided via the
#' `stats` parameter using the specified `engine`.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_change]
#'
#' @name gsw_change
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats Aggregation function with which the data are combined.
#' Default: "mean".
#' @keywords indicator
#' @returns A function that returns a tibble with a column for the aggregated
#'   GSW change indicator.
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
#'   verbose = FALSE
#' )
#'
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_global_surface_water_change()) %>%
#'   calc_indicators(
#'     calc_gsw_change(engine = "extract", stats = "mean")
#'   ) %>%
#'   tidyr::unnest(gsw_change)
#'
#' aoi
#' }
calc_gsw_change <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           global_surface_water_change = NULL,
           name = "gsw_change",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(global_surface_water_change)) {
      return(NA)
    }

    global_surface_water_change <- terra::clamp(
      global_surface_water_change,
      lower = 0,
      upper = 200,
      values = FALSE
    )

    results <- select_engine(
      x = x,
      raster = global_surface_water_change,
      stats = stats,
      engine = engine,
      name = "global_surface_water_change",
      mode = "asset"
    )

    results
  }
}

register_indicator(
  name = "gsw_change",
  description = "Statistics of the surface water change layer by JRC",
  resources = "global_surface_water_change"
)
