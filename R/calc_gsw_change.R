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

  results <- mapme.biodiversity:::.select_engine(
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
