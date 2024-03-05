#' Calculate Global Surface Water (GSW) Seasonality
#'
#' GSW seasonality describes the intra-annual distribution of surface water for
#' each pixel. The raster files have integer cell values between \code{[0, 12]},
#' indicating how many months per year the pixel was classified as water.
#'
#' The pixel values are aggregated using method provided via the
#' \code{stats} parameter.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_seasonality]
#'
#' @name gsw_seasonality
#' @keywords indicator
#' @returns A function that returns a tibble with one column \code{months}
#'   and one column \code{area}, representing the area covered by each class in
#'   ha.
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
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_global_surface_water_seasonality()) %>%
#'   calc_indicators(calc_gsw_seasonality()) %>%
#'   tidyr::unnest(gsw_seasonality)
#'
#' aoi
#' }
calc_gsw_seasonality <- function() {
  function(x,
           global_surface_water_seasonality = NULL,
           name = "gsw_seasonality",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
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
}

register_indicator(
  name = "gsw_seasonality",
  description = "Areal statistic of surface water by seasonality",
  resources = "global_surface_water_seasonality"
)
