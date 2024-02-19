#' Calculate Global Surface Water (GSW) Recurrence
#'
#' Water Recurrence is a measurement of the degree of variability in the
#' presence of water from year to year. It describes the frequency with which
#' water returned to a particular location from one year to another, and is
#' expressed as a percentage. The raster files have integer cell values between
#' \code{[0, 100]}, where 100 represents that water reoccurs predictably every
#' year, whereas lower values indicate that water only occurs episodically.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats_gsw} parameter.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_recurrence]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_gsw}{The aggregation function applied to the input raster
#'   values. Defaults to \code{mean}.}
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
#'   calc_indicators("gsw_recurrence")
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
#' The pixel values are aggregated using method provided via the
#' \code{stats_gsw} parameter.
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_recurrence The GSW Recurrence data source.
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats_gsw Aggregation function with which the data are combined.
#' Default: "mean".
#' @return A tibble containing the aggregated recurrence indicator. The column
#' name is a concatenation of "global_surface_water_recurrence_" +
#' \code{stats_gsw}.
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_recurrence <- function(x,
                                 global_surface_water_recurrence,
                                 engine = "extract",
                                 stats_gsw = "mean",
                                 verbose = TRUE,
                                 ...) {
  if (is.null(global_surface_water_recurrence)) {
    return(NA)
  }

  global_surface_water_recurrence <- terra::clamp(
    global_surface_water_recurrence,
    lower = 0,
    upper = 100,
    values = FALSE
  )

  results <- .select_engine(
    x = x,
    raster = global_surface_water_recurrence,
    stats = stats_gsw,
    engine = engine,
    name = "global_surface_water_recurrence",
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
    stats_gsw = "mean"
  ),
  processing_mode = "asset"
)
