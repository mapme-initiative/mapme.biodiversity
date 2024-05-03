#' Calculate elevation statistics
#'
#' This function allows to calculate elevation statistics for
#' polygons. For each polygon, the desired statistic(s)
#' are returned.
#'
#' The required resources for this indicator are:
#'  - [nasa_srtm]
#'
#' @name elevation
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character "mean", "median" or "sd".
#' @keywords indicator
#' @returns A function that returns a tibble with a column for each statistics.
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
#'   get_resources(get_nasa_srtm()) %>%
#'   calc_indicators(
#'     calc_elevation(engine = "extract", stats = c("mean", "median", "sd", "var"))
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_elevation <- function(engine = "extract",
                           stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           nasa_srtm,
           name = "elevation",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(nasa_srtm)) {
      return(NULL)
    }

    result <- select_engine(
      x = x,
      raster = nasa_srtm,
      stats = stats,
      engine = engine,
      name = "elevation",
      mode = "asset"
    )

    result %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable") %>%
      dplyr::mutate(
        datetime = as.Date("2000-02-01"),
        unit = "m"
      ) %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

register_indicator(
  name = "elevation",
  description = "Statistics of elevation based on NASA SRTM",
  resources = "nasa_srtm"
)
