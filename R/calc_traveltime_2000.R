#' Calculate accessibility statistics for the year 2000
#'
#' Accessibility refers to the ease with which cities can be reached from a
#' certain location. This function allows efficient calculation of accessibility
#' statistics (i.e., travel time to the nearest city) for polygons.  For each
#' polygon, the desired statistic/s (mean, median or sd) is/are returned.
#'
#' The required resource for this indicator is:
#'  - [accessibility_2000]
#'
#' @name traveltime_2000
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum", "var".
#' @keywords indicator
#' @return A function that returns an indicator tibble with accessibility statistics
#'   for the year 2000 as variables and corresponding values (in minutes) as values.
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
#'   get_resources(get_accessibility_2000()) %>%
#'   calc_indicators(
#'     calc_traveltime_2000(stats = c("mean", "median", "sd"), engine = "extract")
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_traveltime_2000 <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           accessibility_2000 = NULL,
           name = "traveltime_2000",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {

    # Ensure the resource is provided
    if (is.null(accessibility_2000)) {
      return(NULL)
    }

    # Set NoData value given by gdalinfo as NULL
    accessibility_2000 <- clamp(accessibility_2000, lower = -2147483646, upper = Inf, values = FALSE)

    # Process the resource using the selected engine and compute the statistics
    result <- select_engine(
      x = x,
      raster = accessibility_2000,
      stats = stats,
      engine = engine,
      name = "traveltime_2000",
      mode = "asset"
    )

    # Return the results in a long format with metadata
    result %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable") %>%
      dplyr::mutate(
        datetime = as.POSIXct("2000-01-01T00:00:00Z"),
        unit = "minutes"
      ) %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

register_indicator(
  name = "traveltime_2000",
  description = "Statistics of traveltime to the closests city in 2000",
  resources = "accessibility_2000"
)
