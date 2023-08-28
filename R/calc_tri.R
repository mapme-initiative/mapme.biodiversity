#' Calculate Terrain Ruggedness Index (TRI) statistics
#'
#' Terrain Ruggedness Index is a measurement developed by Riley, et al. (1999).
#' The elevation difference between the centre pixel and its eight immediate
#' pixels are squared and then averaged and its square root is taken to get
#' the TRI value. This function allows to efficiently calculate terrain ruggedness
#' index (tri) statistics for polygons. For each polygon, the desired statistic/s
#' (mean, median or sd) is/are returned.
#' The required resources for this indicator are:
#'  - [nasa_srtm]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_tri}{Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name tri
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for terrain ruggedness index statistics (in meters).
#'   The range of index values and corresponding meaning:
#'   (1) 0 - 80 m  :- level surface
#'   (2) 81-116 m  :- nearly level surface
#'   (3) 117-161 m :- slightly rugged surface
#'   (4) 162-239 m :- intermediately rugged surface
#'   (5) 240-497 m :- moderately rugged surface
#'   (6) 498-958 m :- highly rugged surface
#'   (7) 959-4367 m:- extremely rugged surface
#' @references Riley, S. J., DeGloria, S. D., & Elliot, R. (1999). Index that quantifies
#'   topographic heterogeneity. intermountain Journal of sciences, 5(1-4), 23-27.
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
#'     years = 2000:2020,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("nasa_srtm") %>%
#'   calc_indicators("tri", stats_tri = c("mean", "median", "sd", "var"), engine = "extract") %>%
#'   tidyr::unnest(tri)
#'
#' aoi
#' }
NULL

#' Calculate Terrain Ruggedness Index (TRI) statistics based on SRTM data sets
#'
#' Considering the 30m resolution SRTM raster datasets users can specify which
#' statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param x A single polygon for which to calculate the tri statistic
#' @param nasa_srtm The elevation raster resource from SRTM
#' @param stats_tri Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character "mean", "median" or "sd".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_tri <- function(x,
                      nasa_srtm,
                      engine = "extract",
                      stats_tri = "mean",
                      verbose = TRUE,
                      ...) {
  # check if input engines are correct
  if (is.null(nasa_srtm)) {
    return(NA)
  }

  tri <- terra::terrain(
    nasa_srtm,
    v = "TRI",
    unit = "degrees",
    neighbors = 8
  )

  .select_engine(
    x = x,
    raster = tri,
    stats = stats_tri,
    engine = engine,
    name = "tri",
    mode = "asset"
  )
}

register_indicator(
  name = "tri",
  resources = list(nasa_srtm = "raster"),
  fun = .calc_tri,
  arguments = list(
    engine = "extract",
    stats_tri = "mean"
  ),
  processing_mode = "asset"
)
