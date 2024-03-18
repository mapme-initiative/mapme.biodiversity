#' Calculate Global Surface Water (GSW) Occurrence
#'
#' GSW occurrence raw data comes in raster files with integer cell values
#' between \code{[0, 100]}. This value gives the percentage of the time that a
#' given pixel was classified as water during the entire observation period. So
#' a 0 denotes a pixel that was never classified as water, 100 denotes a pixel
#' with permanent water.
#'
#' The raw data values are aggregated based on a provided threshold parameter
#' \code{min_occurrence}, the function returns the area covered by values
#' greater or equal than this threshold.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_occurrence]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{min_occurrence}{Threshold value in percentage for pixels values to
#'   contribute to the calculated GSW occurrence area.}
#' }
#'
#' @name gsw_occurrence
#' @docType data
#' @keywords indicator
#' @format A numeric representing the GSW occurrence area.
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
#'   get_resources("global_surface_water_occurrence") %>%
#'   calc_indicators("gsw_occurrence", min_occurrence = 10)
#'
#' aoi
#' }
NULL

#' Calculate Global Surface Water (GSW) Occurrence
#'
#' GSW occurrence raw data comes in raster files with integer cell values
#' between \code{[0, 100]}. This value gives the percentage of the time that a
#' given pixel was classified as water during the entire observation period. So
#' a 0 denotes a pixel that was never classified as water, 100 denotes a pixel
#' with permanent water.
#'
#' The raw data values are aggregated based on a provided threshold parameter
#' \code{min_occurrence}, the function returns the area covered by values
#' greater or equal than this threshold.
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_occurrence The GSW Occurrence data source.
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param min_occurrence Threshold to define which pixels count towards the GSW
#' occurrence area [0, 100].
#' @return A numeric representing the GSW occurrence area.
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_occurrence <- function(x,
                                 global_surface_water_occurrence,
                                 engine = "extract",
                                 min_occurrence = NULL,
                                 verbose = TRUE,
                                 ...) {
  if (is.null(global_surface_water_occurrence)) {
    return(NA)
  }
  stopifnot(
    !is.null(min_occurrence),
    !is.na(min_occurrence),
    is.numeric(min_occurrence),
    min_occurrence >= 0 & min_occurrence <= 100
  )

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

  results <- .select_engine(
    x = x,
    raster = global_surface_water_occurrence,
    stats = "sum",
    engine = engine,
    name = "gsw_occurrence_area",
    mode = "asset"
  )

  return(results)
}

register_indicator(
  name = "gsw_occurrence",
  resources = list(global_surface_water_occurrence = "raster"),
  fun = .calc_gsw_occurrence,
  arguments = list(
    engine = "extract",
    min_occurrence = NULL
  ),
  processing_mode = "asset"
)
