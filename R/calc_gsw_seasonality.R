#' Calculate Global Surface Water (GSW) Seasonality
#'
#' GSW seasonality describes the intra-annual distribution of surface water for
#' each pixel. The raster files have integer cell values between \code{[0, 12]},
#' indicating how many months per year the pixel was classified as water.
#'
#' The pixel values are aggregated by summing up the area covered by each GSW
#' seasonality class. The resulting \code{tibble} always contains 12 rows, one
#' for each month.
#'
#' The required resources for this indicator are:
#'  - [global_surface_water_seasonality]
#'
#' @name gsw_seasonality
#' @docType data
#' @keywords indicator
#' @format A tibble with one column \code{month} and one column \code{area},
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
#' seasonality class. The resulting \code{tibble} always contains 12 rows, one
#' for each month.
#'
#' @param x A single polygon for which to calculate the GSW statistics.
#' @param global_surface_water_seasonality The GSW Seasonality data source.
#' @param engine The preferred processing functions from either one of "zonal",
#' "extract" or "exactextract". Default: "extract".
#' @format A tibble with one column \code{month} and one column \code{area},
#' representing the area covered by each class in ha.
#' @keywords internal
#' @include register.R
#' @noRd
.calc_gsw_seasonality <- function(x,
                                  global_surface_water_seasonality,
                                  engine = "extract",
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

  pixel_areas <- terra::cellSize(
    global_surface_water_seasonality,
    mask = TRUE,
    unit = "ha"
  )

  results <- sapply(1:12, function(month, seasonality_rast, area_rast, aoi) {
    rcl <- matrix(c(month, 1), ncol = 2)
    bitmap_month <- terra::classify(seasonality_rast, rcl = rcl, others = NA)
    area_rast <- terra::mask(
      area_rast,
      bitmap_month
    )

    area_num <- .select_engine(
      x = aoi,
      raster = area_rast,
      stats = "sum",
      engine = engine,
      name = "global_surface_water_seasonality",
      mode = "asset"
    )

    area_num <- as.numeric(area_num)
    if (
      is.null(area_num) |
      is.na(area_num) |
      is.nan(area_num)
    ) {
      area_num <- 0
    }
    return(as.numeric(area_num))
  },
  seasonality_rast = global_surface_water_seasonality,
  area_rast = pixel_areas,
  aoi = x)

  results_tbl <- tibble::tibble(
    month = 1:12,
    area = results
  )

  return(results_tbl)
}

register_indicator(
  name = "gsw_seasonality",
  resources = list(global_surface_water_seasonality = "raster"),
  fun = .calc_gsw_seasonality,
  arguments = list(
    engine = "extract"
  ),
  processing_mode = "asset"
)
