#' Calculate Global Surface Water (GSW) Recurrence
#'
#' Water Recurrence is a measurement of the degree of variability in the
#' presence of water from year to year. It describes the frequency with which
#' water returned to a particular location from one year to another, and is
#' expressed as a percentage. The raster files have integer cell values between
#' `[0, 100]`, where 100 represents that water reoccurs predictably every year,
#' whereas lower values indicate that water only occurs episodically.
#'
#' The raw data values are aggregated based on a provided threshold parameter
#' `min_recurrence`, the function returns the area covered by values
#' greater or equal than this threshold.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_recurrence]

#' @name gsw_recurrence
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param min_recurrence Threshold to define which pixels count towards the GSW
#' recurrence area `[0, 100]`.
#' @keywords indicator
#' @returns A function that returns a tibble with a column for the aggregated
#'   GSW recurrence indicator.
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
#'   get_resources(get_global_surface_water_recurrence()) %>%
#'   calc_indicators(
#'     calc_gsw_recurrence(engine = "extract", min_recurrence = 10)
#'   ) %>%
#'   tidyr::unnest(gsw_recurrence)
#'
#' aoi
#' }
calc_gsw_recurrence <- function(engine = "extract", min_recurrence = NULL) {
  engine <- check_engine(engine)

  stopifnot(
    !is.null(min_recurrence),
    !is.na(min_recurrence),
    is.numeric(min_recurrence),
    min_recurrence >= 0 & min_recurrence <= 100
  )

  function(x,
           global_surface_water_recurrence = NULL,
           name = "gsw_recurrence",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(global_surface_water_recurrence)) {
      return(NA)
    }

    rcl <- matrix(
      c(min_recurrence, 100, 1),
      ncol = 3
    )

    global_surface_water_recurrence <- terra::classify(
      x = global_surface_water_recurrence,
      rcl = rcl,
      include.lowest = TRUE,
      right = NA,
      others = NA
    )

    global_surface_water_recurrence <- terra::cellSize(
      global_surface_water_recurrence,
      mask = TRUE,
      unit = "ha"
    )

    results <- select_engine(
      x = x,
      raster = global_surface_water_recurrence,
      stats = "sum",
      engine = engine,
      name = "gsw_recurrence_area",
      mode = "asset"
    )

    results
  }
}

register_indicator(
  name = "gsw_recurrence",
  description = "Areal statistic of surface water based on reccurence threshold",
  resources = "global_surface_water_recurrence"
)
