#' Calculate Global Surface Water (GSW) Seasonality
#'
#' GSW seasonality describes the intra-annual distribution of surface water for
#' each pixel. The raster files have integer cell values between \code{[0, 12]},
#' indicating how many months per year the pixel was classified as water.
#'
#' The pixel values are aggregated by summing up the area covered by each GSW
#' seasonality class. The resulting \code{tibble} always contains 13 rows, one
#' for each seasonality class.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_seasonality]
#'
#' @name gsw_seasonality
#' @docType data
#' @keywords indicator
#' @format A tibble with one column \code{months} and one column \code{area},
#' representing the area covered by each class in ha.
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
#' The pixel values are aggregated by summing up the area covered by each GSW
#' seasonality class. The resulting \code{tibble} always contains 13 rows, one
#' for each seasonality class.
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_seasonality The GSW Seasonality data source.
#' @format A tibble with one column \code{months} and one column \code{area},
#' representing the area covered by each class in ha.
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_seasonality <- function(x,
                                  global_surface_water_seasonality,
                                  ...) {
  if (is.null(global_surface_water_seasonality)) {
    return(NA)
  }

  global_surface_water_seasonality <- terra::mask(
    global_surface_water_seasonality,
    x
  )

  global_surface_water_seasonality <- terra::clamp(
    global_surface_water_seasonality,
    lower = 0,
    upper = 12,
    values = FALSE
  )

  pixel_areas <- terra::cellSize(
    global_surface_water_seasonality,
    mask = TRUE,
    unit = "ha"
  )

  res_zonal <- terra::zonal(
    pixel_areas,
    global_surface_water_seasonality,
    fun = "sum"
  )
  names(res_zonal) <- c("value", "area")

  result <- tibble::tibble(
    month = 0:12
  )

  result <- merge(
    result,
    res_zonal,
    by.x = "month",
    by.y = "value",
    all.x = TRUE
  )
  result$area[is.na(result$area)] <- 0

  return(tibble::tibble(result))
}

register_indicator(
  name = "gsw_seasonality",
  resources = list(global_surface_water_seasonality = "raster"),
  fun = .calc_gsw_seasonality,
  arguments = list(),
  processing_mode = "asset"
)
