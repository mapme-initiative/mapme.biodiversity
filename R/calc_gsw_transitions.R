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
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for name of the transition classes and corresponding area (in ha).
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
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2001,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("global_surface_water_transitions") %>%
#'   calc_indicators("gsw_transitions")
#'
#' aoi
#' }
NULL

#' Calculate Global Surface Water (GSW) Transitions
#'
#' GSW transition data contains information about the type of surface water
#' change for each pixel. The raster files have integer cell values between
#' [0, 10] that code for different transition classes:
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
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_transitions The GSW Transitions data source.
#' @return A tibble with two columns
#' \itemize{
#'   \item class: Surface water transition class.
#'   \item area: Area in ha.
#' }
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_transitions <- function(x,
                                  global_surface_water_transitions,
                                  verbose = TRUE,
                                  ...) {
  if (is.null(global_surface_water_transitions)) {
    return(NA)
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

  result <- purrr::map_dfr(seq_len(terra::nlyr(transition_mask)), function(i) {
    terra::zonal(arearaster, transition_mask[[i]], sum) %>%
      stats::setNames(c("code", "area")) %>%
      dplyr::left_join(.gsw_transition_classes, by = "code") %>%
      dplyr::select(class, area)
  }) %>%
    tibble::tibble()

  return(result)
}

.gsw_transition_classes <- data.frame(
  code = 1:10,
  class = c(
    "Permanent", "New Permanent", "Lost Permanent", "Seasonal",
    "New Seasonal", "Lost Seasonal", "Seasonal to Permanent",
    "Permanent to Seasonal", "Ephemeral Permanent", "Ephemeral Seasonal"
  )
)

register_indicator(
  name = "gsw_transitions",
  resources = list(global_surface_water_transitions = "raster"),
  fun = .calc_gsw_transitions,
  arguments = list(
    engine = "extract"
  ),
  processing_mode = "asset"
)
