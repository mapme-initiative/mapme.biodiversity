.calc_gsw_recurrence <- function(x,
                                   global_surface_water_recurrence,
                                   engine = "extract",
                                   stats_gsw = "mean",
                                   verbose = TRUE,
                                   ...) {
  if (is.null(global_surface_water_recurrence)) {
    return(NA)
  }

  global_surface_water_recurrence <- terra::clamp(
    global_surface_water_recurrence,
    lower = 0,
    upper = 100,
    values = FALSE
  )

  results <- mapme.biodiversity:::.select_engine(
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
