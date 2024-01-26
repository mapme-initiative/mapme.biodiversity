#' Calculate Global Surface Water (GSW) Change
#'
#' The change in water occurrence intensity between the two periods is derived
#' from homologous pairs of months (i.e. same months containing valid
#' observations in both periods). The difference in the occurrence of surface
#' water was calculated for each homologous pair of months. The average of all
#' of these differences constitutes the Surface Water Occurrence change
#' intensity. The raster files have integer cell values between [0, 200] where 0
#' represents surface water loss and 200 represents surface water gain.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats_gsw} parameter.
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_change The GSW Change data source.
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats_gsw Aggregation function with which the data are combined.
#' Default: "mean".
#' @return A tibble containing the aggregated GSW change indicator. The column
#' name is a concatenation of "global_surface_water_change_" + \code{stats_gsw}.
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_change <- function(x,
                             global_surface_water_change,
                             engine = "extract",
                             stats_gsw = "mean",
                             verbose = TRUE,
                             ...) {
  if (is.null(global_surface_water_change)) {
    return(NA)
  }

  global_surface_water_change <- terra::clamp(
    global_surface_water_change,
    lower = 0,
    upper = 200,
    values = FALSE
  )

  results <- .select_engine(
    x = x,
    raster = global_surface_water_change,
    stats = stats_gsw,
    engine = engine,
    name = "global_surface_water_change",
    mode = "asset"
  )

  results
}

register_indicator(
  name = "gsw_change",
  resources = list(global_surface_water_change = "raster"),
  fun = .calc_gsw_change,
  arguments = list(
    engine = "extract",
    stats_gsw = "mean"
  ),
  processing_mode = "asset"
)
