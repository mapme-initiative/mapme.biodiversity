#' Calculate accessibility statistics
#'
#' Accessibility is the ease with which larger cities can be reached from a
#' certain location. This function allows to efficiently calculate accessibility
#' statistics (i.e. travel time to nearby major cities) for polygons. For each
#' polygon, the desired statistic/s (mean, median or sd) is/are returned.
#'
#' The required resources for this indicator are:
#'  - [nelson_et_al]
#'
#' @name traveltime
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".
#' @keywords indicator
#' @returns A function that returns an indicator tibble with city ranges and
#'   statisics as variable and corresponding values (in minutes) as value.
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
#'   get_resources(get_nelson_et_al(ranges = "100k_200k")) %>%
#'   calc_indicators(
#'     calc_traveltime(engine = "extract", stats = c("min", "max"))
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_traveltime <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           nelson_et_al = NULL,
           name = "traveltime",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(nelson_et_al)) {
      return(NULL)
    }
    # set max value of 65535 to NA
    nelson_et_al <- clamp(nelson_et_al, lower = -Inf, upper = 65534, values = FALSE)
    results <- select_engine(
      x = x,
      raster = nelson_et_al,
      stats = stats,
      engine = engine,
      name = "traveltime",
      mode = "asset"
    )

    distances <- unlist(
      lapply(
        names(nelson_et_al),
        function(x) strsplit(x, "-|.tif")[[1]][2]
      )
    )

    results %>%
      dplyr::mutate(distances = distances) %>%
      tidyr::pivot_longer(-distances, names_to = "variable") %>%
      dplyr::mutate(
        datetime = as.POSIXct("2015-01-01T00:00:00Z"),
        variable = paste0(distances, "_", variable),
        unit = "minutes"
      ) %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

register_indicator(
  name = "traveltime",
  description = "Statistics of traveltime to the clostes city grouped by city category",
  resources = "nelson_et_al"
)
