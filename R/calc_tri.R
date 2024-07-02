#' Calculate Terrain Ruggedness Index (TRI) statistics
#'
#' Terrain Ruggedness Index is a measurement developed by Riley, et al. (1999).
#' The elevation difference between the centre pixel and its eight immediate
#' pixels are squared and then averaged and its square root is taken to get
#' the TRI value. This function allows to calculate terrain ruggedness
#' index (tri) statistics for polygons. For each polygon, the desired statistic(s)
#' are returned.
#'
#'  The range of index values and corresponding meaning:
#'   - 0-80 m - level surface
#'   - 81-116 m - nearly level surface
#'   - 117-161 m - slightly rugged surface
#'   - 162-239 m - intermediately rugged surface
#'   - 240-497 m - moderately rugged surface
#'   - 498-958 m - highly rugged surface
#'   - 959-4367 m  extremely rugged surface
#'
#' The required resources for this indicator are:
#'  - [nasa_srtm]
#'
#' @name tri
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".
#' @keywords indicator
#' @returns A function that returns an indicator tibble with tri as variable and
#'   the respective statistic as value.
#'
#' @references Riley, S. J., DeGloria, S. D., & Elliot, R. (1999). Index that quantifies
#'   topographic heterogeneity. Intermountain Journal of Sciences, 5(1-4), 23-27.
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
#'     calc_tri(stats = c("mean", "median", "sd", "var"), engine = "extract")
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_tri <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           nasa_srtm = NULL,
           name = "tri",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(nasa_srtm)) {
      return(NULL)
    }

    tri <- terra::terrain(
      nasa_srtm,
      v = "TRI",
      unit = "degrees",
      neighbors = 8
    )

    result <- select_engine(
      x = x,
      raster = tri,
      stats = stats,
      engine = engine,
      name = "tri",
      mode = "asset"
    )

    result %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable") %>%
      dplyr::mutate(
        datetime = as.POSIXct("2000-02-01T00:00:00Z"),
        unit = "m"
      ) %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

register_indicator(
  name = "tri",
  description = "Statistics of terrain rudgedness index based on NASA SRTM DEM",
  resources = "nasa_srtm"
)
