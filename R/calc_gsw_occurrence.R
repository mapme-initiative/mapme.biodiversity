#' Calculate Global Surface Water (GSW) Occurrence
#'
#' GSW occurrence raw data comes in raster files with integer cell values
#' between \code{[0, 100]}. This value gives the percentage of the time that a
#' given pixel was classified as water during the entire observation period. So
#' a 0 denotes a pixel that was never classified as water, 100 denotes a pixel
#' with permanent water.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats} parameter.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_occurrence]
#'
#' @name gsw_occurrence
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats Aggregation function with which the data are combined.
#' Default: "mean".
#' @keywords indicator
#' @returns A tibble with a column for the aggregated GSW occurrence indicator.
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
#'   tmpdir = tempdir(),
#'   verbose = FALSE
#' )
#'
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_global_surface_water_occurrence()) %>%
#'   calc_indicators(
#'     calc_gsw_occurrence(engine = "extract", stats = "mean")
#'   ) %>%
#'   tidyr::unnest(gsw_occurence)
#'
#' aoi
#' }
calc_gsw_occurrence <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           global_surface_water_occurrence = NULL,
           name = "gsw_occurence",
           mode = "asset",
           rundir = mapme_options()[["tempdir"]],
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(global_surface_water_occurrence)) {
      return(NA)
    }

    global_surface_water_occurrence <- terra::clamp(
      global_surface_water_occurrence,
      lower = 0,
      upper = 100,
      values = FALSE
    )

    results <- select_engine(
      x = x,
      raster = global_surface_water_occurrence,
      stats = stats,
      engine = engine,
      name = "global_surface_water_occurrence",
      mode = "asset"
    )

    results
  }
}

register_indicator(
  name = "gsw_occurrence",
  resources = list(global_surface_water_occurrence = "raster")
)
