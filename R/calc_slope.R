#' Calculate slope statistics
#'
#' This function allows to calculate slope statistics for
#' polygons. For each polygon, the desired statistic(s)
#' are returned.
#'
#' The required resource for this indicator is:
#'  - [nasa_srtm]
#'
#' @name slope
#' @param engine The preferred processing function from either one of "zonal",
#'    "extract" or "exactextract" as a character string.
#' @param stats Function to be applied to compute statistics for polygons.
#'    Accepts either a single string or a vector of strings, such as "mean",
#'    "median", "sd", "min", "max", "sum", or "var".
#' @keywords indicator
#' @return A function that returns an indicator tibble with specified slope
#'    statistics as variables and corresponding values (in degrees).
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
#'     calc_slope(stats = c("mean", "median", "sd", "var"), engine = "extract")
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_slope <- function(engine = "exactextract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           nasa_srtm = NULL,
           name = "slope",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(nasa_srtm)) {
      return(NULL)
    }

    # Calculate the slope using terra::terrain
    slope_raster <- terra::terrain(
      nasa_srtm,
      v = "slope",       # Slope calculation
      unit = "degrees",  # Slope in degrees
      neighbors = 8      # Default to 8 neighbors
    )

    result <- select_engine(
      x = x,
      raster = slope_raster,
      stats = stats,
      engine = engine,
      name = "slope",
      mode = mode
    )

    result %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable") %>%
      dplyr::mutate(
        datetime = as.POSIXct("2000-02-01T00:00:00Z"),
        unit = "degrees"
      ) %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

register_indicator(
  name = "slope",
  description = "Statistics of slope based on NASA SRTM",
  resources = "nasa_srtm"
)

