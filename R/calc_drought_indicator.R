#' Calculate drought indicator statistics
#'
#' This function allows to efficiently calculate the relative wetness in the
#' shallow groundwater section with regard to the the 1948-2012 reference period.
#' The values represent the wetness percentile a given area achieves at a given
#' point in time in regard to the reference period.
#' For each polygon, the desired statistic/s (mean, median or sd) is/are
#' returned.
#'
#' The required resources for this indicator are:
#'  - [nasa_grace]
#'
#' @name drought_indicator
#' @param stats Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "mean", "median" or "sd".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @keywords indicator
#' @returns A function that returns a tibble with a column for each specified
#'   stats and a column with the respective date.
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
#'   get_resources(get_nasa_grace(years = 2022)) %>%
#'   calc_indicators(
#'     calc_drought_indicator(
#'       engine = "extract",
#'       stats = c("mean", "median")
#'     )
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_drought_indicator <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           nasa_grace = NULL,
           name = "drought_indicator",
           mode = "portfolio",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    # check if input engines are correct
    if (is.null(nasa_grace)) {
      return(NULL)
    }
    results <- select_engine(
      x = x,
      raster = nasa_grace,
      stats = stats,
      engine = engine,
      name = "wetness",
      mode = mode
    )

    dates <- sub(".*(\\d{8}).*", "\\1", names(nasa_grace))
    dates <- as.Date(dates, format = "%Y%m%d")

    prep_results <- function(result, datetimes) {
      result %>%
        dplyr::mutate(datetime = datetimes, unit = "dimensionless") %>%
        tidyr::pivot_longer(cols = -c(datetime, unit), names_to = "variable") %>%
        dplyr::select(datetime, variable, unit, value)
    }

    if (mode == "portfolio") {
      results <- purrr::map(results, prep_results, datetimes = dates)
    } else {
      results <- prep_results(results, date)
    }
    results
  }
}

register_indicator(
  name = "drought_indicator",
  description = "Relative wetness statistics based on NASA GRACE",
  resources = "nasa_grace"
)
