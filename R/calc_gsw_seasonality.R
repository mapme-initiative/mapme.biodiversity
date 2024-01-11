.calc_gsw_seasonality <- function(x,
                                   global_surface_water_seasonality,
                                   engine = "extract",
                                   stats_gsw = "mean",
                                   verbose = TRUE,
                                   ...) {
  if (is.null(global_surface_water_seasonality)) {
    return(NA)
  }

  global_surface_water_seasonality <- terra::clamp(
    global_surface_water_seasonality,
    lower = 0,
    upper = 12,
    values = FALSE
  )

  results <- mapme.biodiversity:::.select_engine(
    x = x,
    raster = global_surface_water_seasonality,
    stats = stats_gsw,
    engine = engine,
    name = "global_surface_water_seasonality",
    mode = "asset"
  )

  results
}

register_indicator(
  name = "gsw_seasonality",
  resources = list(global_surface_water_seasonality = "raster"),
  fun = .calc_gsw_seasonality,
  arguments = list(
    engine = "extract",
    stats_gsw = "mean"
  ),
  processing_mode = "asset"
)
