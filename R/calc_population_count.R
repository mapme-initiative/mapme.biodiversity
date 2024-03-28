#' Calculate population count statistics
#'
#' WorldPop, which was initiated in 2013, offers easy access to spatial demographic
#' datasets, claiming to use peer-reviewed and fully transparent methods to create
#' global mosaics for the years 2000 to 2020. This function allows to efficiently
#' calculate population count statistics (e.g. total number of population) for
#' polygons. For each polygon, the desired statistic/s (min, max, sum, mean,
#' median, sd or var) is/are returned.
#'
#' The required resources for this indicator are:
#'  - [worldpop]
#'
#' @name population_count
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
#' @keywords indicator
#' @returns A function that returns tibble with a column for population count
#'   statistics.
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
#'   get_resources(get_worldpop(years = 2000:2010)) %>%
#'   calc_indicators(
#'     calc_population_count(engine = "extract", stats = c("sum", "median"))
#'   ) %>%
#'   tidyr::unnest(population_count)
#'
#' aoi
#' }
calc_population_count <- function(engine = "extract", stats = "sum") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           worldpop = NULL,
           name = "population_count",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(worldpop)) {
      return(NA)
    }

    # set max value of 65535 to NA
    worldpop <- clamp(
      worldpop,
      lower = -Inf,
      upper = 65534,
      values = FALSE
    )

    results <- select_engine(
      x = x,
      raster = worldpop,
      stats = stats,
      engine = engine,
      name = "population_count",
      mode = "asset"
    )

    years <- unlist(lapply(names(worldpop), function(x) strsplit(x, "_")[[1]][2]))
    results$year <- years
    results
  }
}

register_indicator(
  name = "population_count",
  description = "Statistic of population counts",
  resources = "worldpop"
)
