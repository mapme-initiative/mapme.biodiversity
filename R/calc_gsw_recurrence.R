#' Calculate Global Surface Water (GSW) Recurrence
#'
#' Water Recurrence is a measurement of the degree of variability in the
#' presence of water from year to year. It describes the frequency with which
#' water returned to a particular location from one year to another, and is
#' expressed as a percentage. The raster files have integer cell values between
#' [0, 100], where 100 represents that water reoccurs predictably every year,
#' whereas lower values indicate that water only occurs episodically.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats_gsw} parameter.
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_recurrence The GSW Recurrence data source.
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats_gsw Aggregation function with which the data are combined.
#' Default: "mean".
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_recurrence <- function(x,
                                 global_surface_water_recurrence,
                                 engine = "extract",
                                 stats_gsw = "mean") {
  if (is.null(global_surface_water_recurrence)) {
    return(NA)
  }

  global_surface_water_recurrence <- terra::clamp(
    global_surface_water_recurrence,
    lower = 0,
    upper = 100,
    values = FALSE
  )

  results <- .select_engine(
    x = x,
    raster = global_surface_water_recurrence,
    stats = stats_gsw,
    engine = engine,
    name = "global_surface_water_recurrence",
    mode = "asset"
  )

  results
}

register_indicator(
  name = "gsw_recurrence",
  resources = list(global_surface_water_recurrence = "raster"),
  fun = .calc_gsw_recurrence,
  arguments = list(
    engine = "extract",
    stats_gsw = "mean"
  ),
  processing_mode = "asset"
)
