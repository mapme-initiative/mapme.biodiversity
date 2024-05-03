#' Calculate maximum temperature statistics
#'
#' This function allows to efficiently calculate maximum temperature statistics
#' from Worldclim for polygons. For each polygon, the desired statistic/s (min,
#' max, sum, mean, median, sd or var) is/are returned.
#'
#' The required resources for this indicator are:
#'  - maximum temperature layer from [worldclim_max_temperature]
#'
#' @name temperature_max_wc
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".
#' @keywords indicator
#' @returns A tibble with a column for maximum temperature statistics (in °C)
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
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_worldclim_max_temperature(years = 2018)) %>%
#'   calc_indicators(
#'     calc_temperature_max_wc(
#'       engine = "extract",
#'       stats = c("mean", "median")
#'     )
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_temperature_max_wc <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           worldclim_max_temperature = NULL,
           name = "temperature_max_wc",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(worldclim_max_temperature)) {
      return(NULL)
    }

    results <- .calc_worldclim(
      x = x,
      worldclim = worldclim_max_temperature,
      engine = engine,
      stats = stats,
      unit = "C"
    )
    results
  }
}

register_indicator(
  name = "temperature_max_wc",
  description = "Statistics of WorldClim maximum temperature layer",
  resources = "worldclim_max_temperature"
)
