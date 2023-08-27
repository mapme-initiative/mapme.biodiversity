#' Calculate population count statistics
#'
#' WorldPop, which was initiated in 2013, offers easy access to spatial demographic
#' datasets, claiming to use peer-reviewed and fully transparent methods to create
#' global mosaics for the years 2000 to 2020. This function allows to efficiently
#' calculate population count statistics (e.g. total number of population) for
#' polygons. For each polygon, the desired statistic/s (min, max, sum, mean,
#' median, sd or var) is/are returned.
#' The required resources for this indicator are:
#'  - [worldpop]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_popcount}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name population_count
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for population count statistics
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
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2000:2010,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("worldpop") %>%
#'   calc_indicators("population_count", stats_popcount = c("sum", "median"), engine = "extract") %>%
#'   tidyr::unnest(population_count)
#'
#' aoi
#' }
NULL

#' Calculate population count statistics
#'
#' Considering the 1km WorldPop Unconstrained Global Mosaics raster datasets users
#' can specify which statistics among min, max, sum, mean, median, standard deviation
#' or var to compute. Also, users can specify the functions i.e. zonal from package
#' terra, extract from package terra, or exactextract from exactextractr as desired.
#'
#' @param x A single polygon for which to calculate the population count statistic
#' @param worldpop The population count raster resource from worldPop
#' @param stats_popcount Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_population_count <- function(x,
                                   worldpop,
                                   engine = "extract",
                                   stats_popcount = "sum",
                                   verbose = TRUE,
                                   ...) {
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

  results <- .select_engine(
    x = x,
    raster = worldpop,
    stats = stats_popcount,
    engine = engine,
    name = "popcount",
    mode = "asset"
  )

  years <- unlist(lapply(names(worldpop), function(x) strsplit(x, "_")[[1]][2]))
  results$year <- years
  results
}

register_indicator(
  name = "population_count",
  resources = list(worldpop = "raster"),
  fun = .calc_population_count,
  arguments = list(
    engine = "extract",
    stats_popcount = "sum"
  ),
  processing_mode = "asset"
)
