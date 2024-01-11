.calc_gsw_transitions <- function(x, global_surface_water_transitions,
                                   ...) {
  if (is.null(global_surface_water_transitions)) {
    return(NA)
  }

  classes <- NULL

  global_surface_water_transitions <- terra::clamp(
    global_surface_water_transitions,
    lower = 1,
    upper = 10,
    values = FALSE
  )

  x_v <- terra::vect(x)
  transition_mask <- terra::mask(global_surface_water_transitions, x_v)
  arearaster <- terra::cellSize(transition_mask, mask = TRUE, unit = "ha")

  result <- purrr::map_dfr(seq_len(terra::nlyr(transition_mask)), function(i) {
    terra::zonal(arearaster, transition_mask[[i]], sum) %>%
      setNames(c("code", "area")) %>%
      dplyr::left_join(.gsw_transition_classes, by = "code") %>%
      dplyr::select(class, area)
  }) %>% tibble::tibble()

  return(result)
}

.gsw_transition_classes <- data.frame(
  code = 1:10,
  class = c("Permanent", "New Permanent", "Lost Permanent", "Seasonal", "New Seasonal",
    "Lost Seasonal", "Seasonal to Permanent", "Permanent to Seasonal",
    "Ephemeral Permanent", "Ephemeral Seasonal"
  )
)

register_indicator(
  name = "gsw_transitions",
  resources = list(global_surface_water_transitions = "raster"),
  fun = .calc_gsw_transitions,
  arguments = list(
    engine = "extract",
    stats_gsw = "mean"
  ),
  processing_mode = "asset"
)
