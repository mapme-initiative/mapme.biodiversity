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

  results <- mapme.biodiversity:::.select_engine(
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
