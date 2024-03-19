#' Calculate Global Surface Water (GSW) Recurrence
#'
#' Water Recurrence is a measurement of the degree of variability in the
#' presence of water from year to year. It describes the frequency with which
#' water returned to a particular location from one year to another, and is
#' expressed as a percentage. The raster files have integer cell values between
#' \code{[0, 100]}, where 100 represents that water reoccurs predictably every
#' year, whereas lower values indicate that water only occurs episodically.
#'
#' The raw data values are aggregated based on a provided threshold parameter
#' \code{min_recurrence}, the function returns the area covered by values
#' greater or equal than this threshold.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_recurrence]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{min_recurrence}{Threshold value in percentage for pixels values to
#'   contribute to the calculated GSW recurrence area.}
#' }
#'
#' @name gsw_recurrence
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for the aggregated GSW recurrence indicator.
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
#'   get_resources("global_surface_water_recurrence") %>%
#'   calc_indicators("gsw_recurrence", min_recurrence = 10)
#'
#' aoi
#' }
NULL

#' Calculate Global Surface Water (GSW) Recurrence
#'
#' Water Recurrence is a measurement of the degree of variability in the
#' presence of water from year to year. It describes the frequency with which
#' water returned to a particular location from one year to another, and is
#' expressed as a percentage. The raster files have integer cell values between
#' [0, 100], where 100 represents that water reoccurs predictably every year,
#' whereas lower values indicate that water only occurs episodically.
#'
#' The raw data values are aggregated based on a provided threshold parameter
#' \code{min_recurrence}, the function returns the area covered by values
#' greater or equal than this threshold.
#'
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_recurrence The GSW Recurrence data source.
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param min_recurrence Threshold to define which pixels count towards the GSW
#' recurrence area [0, 100].
#' @return A numeric representing the GSW recurrence area.
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_recurrence <- function(x,
                                 global_surface_water_recurrence,
                                 engine = "extract",
                                 min_recurrence = NULL,
                                 verbose = TRUE,
                                 ...) {
  if (is.null(global_surface_water_recurrence)) {
    return(NA)
  }
  stopifnot(
    !is.null(min_recurrence),
    !is.na(min_recurrence),
    is.numeric(min_recurrence),
    min_recurrence >= 0 & min_recurrence <= 100
  )

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

  results <- .select_engine(
    x = x,
    raster = global_surface_water_recurrence,
    stats = "sum",
    engine = engine,
    name = "gsw_recurrence_area",
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
    min_recurrence = NULL
  ),
  processing_mode = "asset"
)
