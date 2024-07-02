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
#' @returns A function that returns an indicator tibble with seasonality
#'   categories as variables and corresponding areas (in ha) as value.
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
#'   portfolio_long()
#'
#' aoi
#' }
calc_gsw_seasonality <- function() {
  function(x,
           global_surface_water_seasonality = NULL,
           name = "gsw_seasonality",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(global_surface_water_seasonality)) {
      return(NULL)
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

    names(res_zonal) <- c("variable", "value")

    result <- tibble::tibble(
      variable = 0:12
    )

    result <- merge(
      result,
      res_zonal,
      all.x = TRUE
    )
    result[["value"]][is.na(result[["value"]])] <- 0

    result %>%
      dplyr::mutate(
        variable = paste0("gsw_seasonality_", sprintf("%02d", variable)),
        datetime = as.POSIXct("2021-01-01T00:00:00Z"),
        unit = "ha"
      ) %>%
      dplyr::select(datetime, variable, unit, value) %>%
      tibble::as_tibble()
  }
}

register_indicator(
  name = "gsw_seasonality",
  description = "Areal statistic of surface water by seasonality",
  resources = "global_surface_water_seasonality"
)
