#' Calculate Global Surface Water (GSW) Seasonality
#'
#' GSW seasonality describes the intra-annual distribution of surface water for
#' each pixel. The raster files have integer cell values between \code{[0, 12]},
#' indicating how many months per year the pixel was classified as water.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats_gsw} parameter.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_seasonality]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_gsw}{The aggregation function applied to the input raster
#'   values. Defaults to \code{mean}.}
#' }
#'
#' @name gsw_seasonality
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for the aggregated GSW seasonality indicator.
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
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2001,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("global_surface_water_seasonality") %>%
#'   calc_indicators("gsw_seasonality")
#'
#' aoi
#' }
NULL

#' Calculate Global Surface Water (GSW) Seasonality
#'
#' GSW seasonality describes the intra-annual distribution of surface water for
#' each pixel. The raster files have integer cell values between [0, 12],
#' indicating how many months per year the pixel was classified as water.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats_gsw} parameter.
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_seasonality The GSW Seasonality data source.
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @param stats_gsw Aggregation function with which the data are combined.
#' Default: "mean".
#' @return A tibble containing the aggregated seasonality indicator. The column
#' name is a concatenation of "global_surface_water_seasonality_" +
#' \code{stats_gsw}.
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_seasonality <- function(x,
                                  global_surface_water_seasonality,
                                  engine = "extract",
                                  stats_gsw = "mean",
                                  verbose = TRUE,
                                  ...) {
  if (is.null(global_surface_water_seasonality)) {
    return(NA)
  }

  global_surface_water_seasonality <- terra::clamp(
    global_surface_water_seasonality,
    lower = 0,
    upper = 12,
    values = FALSE
  )

  results <- .select_engine(
    x = x,
    raster = global_surface_water_seasonality,
    stats = stats_gsw,
    engine = engine,
    name = "global_surface_water_seasonality",
    mode = "asset"
  )

  results
}

register_indicator(
  name = "gsw_seasonality",
  resources = list(global_surface_water_seasonality = "raster"),
  fun = .calc_gsw_seasonality,
  arguments = list(
    engine = "extract",
    stats_gsw = "mean"
  ),
  processing_mode = "asset"
)
