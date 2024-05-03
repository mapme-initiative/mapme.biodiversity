#' Calculate Global Surface Water (GSW) Transitions
#'
#' GSW transition data contains information about the type of surface water
#' change for each pixel. The raster files have integer cell values between
#' \code{[0, 10]} that code for different transition classes:
#'
#' | Value | Transition Class      |
#' |-------|-----------------------|
#' | 1     | Permanent             |
#' | 2     | New Permanent         |
#' | 3     | Lost Permanent        |
#' | 4     | Seasonal              |
#' | 5     | New Seasonal          |
#' | 6     | Lost Seasonal         |
#' | 7     | Seasonal to Permanent |
#' | 8     | Permanent to Seasonal |
#' | 9     | Ephemeral Permanent   |
#' | 10    | Ephemeral Seasonal    |
#'
#' To aggregate, we sum up the area of each transition class for a given region.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_transitions]
#'
#' @name gsw_transitions
#' @keywords indicator
#' @returns A function that returns a tibble with a column for name of the
#'   transition classes and corresponding area (in ha).
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
#'   get_resources(get_global_surface_water_transitions()) %>%
#'   calc_indicators(calc_gsw_transitions()) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_gsw_transitions <- function() {
  function(x,
           global_surface_water_transitions = NULL,
           name = "gsw_transitions",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(global_surface_water_transitions)) {
      return(NULL)
    }

    global_surface_water_transitions <- terra::clamp(
      global_surface_water_transitions,
      lower = 1,
      upper = 10,
      values = FALSE
    )

    x_v <- terra::vect(x)
    transition_mask <- terra::mask(global_surface_water_transitions, x_v)
    arearaster <- terra::cellSize(transition_mask, mask = TRUE, unit = "ha")

    purrr::map_dfr(seq_len(terra::nlyr(transition_mask)), function(i) {
      terra::zonal(arearaster, transition_mask[[i]], sum) %>%
        stats::setNames(c("code", "value")) %>%
        dplyr::left_join(.gsw_transition_classes, by = "code") %>%
        dplyr::select(variable, value)
    }) %>%
      dplyr::mutate(
        variable = paste0("gsw_", variable),
        datetime = as.Date("2021-01-01"),
        unit = "ha"
      ) %>%
      tibble::as_tibble() %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

.gsw_transition_classes <- data.frame(
  code = 1:10,
  variable = c(
    "permanent", "new_permanent", "lost_permanent", "seasonal",
    "new_seasonal", "lost_seasonal", "seasonal_to_permanent",
    "permanent_to_seasonal", "ephemeral_permanent", "ephemeral_seasonal"
  )
)

register_indicator(
  name = "gsw_transitions",
  description = "Areal statistics of surface water grouped by transition class",
  resources = "global_surface_water_transitions"
)
