#' Calculate Global Surface Water (GSW) Occurrence
#'
#' GSW occurrence raw data comes in raster files with integer cell values
#' between `[0, 100]`. This value gives the percentage of the time that a
#' given pixel was classified as water during the entire observation period. So
#' a 0 denotes a pixel that was never classified as water, 100 denotes a pixel
#' with permanent water.
#'
#' The raw data values are aggregated based on a provided threshold parameter
#' `min_occurrence`, the function returns the area covered by values
#' greater or equal than this threshold.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_occurrence]
#'
#' @name gsw_occurrence
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param min_occurrence Threshold to define which pixels count towards the GSW
#' occurrence area `[0, 100]`.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with occurrence as
#'   variable and the corresponding area (in ha) as value.
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
#'   get_resources(get_global_surface_water_occurrence()) %>%
#'   calc_indicators(
#'     calc_gsw_occurrence(engine = "extract", min_occurrence = 10)
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_gsw_occurrence <- function(engine = "extract", min_occurrence = NULL) {
  engine <- check_engine(engine)
  stopifnot(
    !is.null(min_occurrence),
    !is.na(min_occurrence),
    is.numeric(min_occurrence),
    min_occurrence >= 0 & min_occurrence <= 100
  )

  function(x,
           global_surface_water_occurrence = NULL,
           name = "gsw_occurrence",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(global_surface_water_occurrence)) {
      return(NULL)
    }

    rcl <- matrix(
      c(min_occurrence, 100, 1),
      ncol = 3
    )

    global_surface_water_occurrence <- terra::classify(
      x = global_surface_water_occurrence,
      rcl = rcl,
      include.lowest = TRUE,
      right = NA,
      others = NA
    )

    global_surface_water_occurrence <- terra::cellSize(
      global_surface_water_occurrence,
      mask = TRUE,
      unit = "ha"
    )

    results <- select_engine(
      x = x,
      raster = global_surface_water_occurrence,
      stats = "sum",
      engine = engine,
      name = "gsw_occurrence",
      mode = "asset"
    )

    results %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable") %>%
      dplyr::mutate(
        variable = "gsw_occurrence",
        datetime = as.POSIXct("2021-01-01T00:00:00Z"),
        unit = "ha"
      ) %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

register_indicator(
  name = "gsw_occurrence",
  description = "Areal statistic of surface water based on occurrence threshold",
  resources = "global_surface_water_occurrence"
)
