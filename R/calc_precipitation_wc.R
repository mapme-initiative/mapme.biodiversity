#' Calculate precipitation statistics
#'
#' This function allows to efficiently calculate precipitation statistics
#' from Worldclim for polygons. For each polygon, the desired statistic/s (min,
#' max, sum, mean, median, sd or var) is/are returned.
#'
#' The required resources for this indicator are:
#'  - precipitation layer from [worldclim_precipitation]
#'
#' @name precipitation_wc
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".
#' @docType data
#' @keywords indicator
#' @returns A function that returns a tibble with a column for precipitation
#'   statistics (in mm).
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
#'   get_resources(get_worldclim_precipitation(years = 2018)) %>%
#'   calc_indicators(
#'     calc_precipitation_wc(
#'       engine = "extract",
#'       stats = c("mean", "median")
#'     )
#'   ) %>%
#'   tidyr::unnest(precipitation_wc)
#'
#' aoi
#' }
calc_precipitation_wc <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           worldclim_precipitation = NULL,
           name = "precipitation_wc",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
    results <- .calc_worldclim(
      x = x,
      worldclim = worldclim_precipitation,
      engine = engine,
      stats = stats
    )
    results
  }
}

register_indicator(
  name = "precipitation_wc",
  description = "Statistics of WorldClim precipitation layer",
  resources = "worldclim_precipitation"
)
