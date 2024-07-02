#' Calculate area of different landcover classes
#'
#' The land cover data shows us how much of the region is covered by forests,
#' rivers, wetlands, barren land, or urban infrastructure thus allowing the
#' observation of land cover dynamics over a period of time. This function
#' allows to efficiently calculate area of different landcover classes for
#' polygons. For each polygon, the area of the classes in hectare(ha) is
#' returned.
#'
#' The required resources for this indicator are:
#'  - [esalandcover]
#'
#' @name landcover
#' @keywords indicator
#' @returns A function that returns an indicator tibble with landcover classes
#'   as variables and corresponding areas (in ha) as value.
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
#'   get_resources(get_esalandcover(years = 2016:2017)) %>%
#'   calc_indicators(calc_landcover()) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_landcover <- function() {
  function(x,
           esalandcover = NULL,
           name = "landcover",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    percentage <- NULL
    year <- NULL
    classes <- NULL

    if (is.null(esalandcover)) {
      return(NULL)
    }

    x_v <- vect(x)
    esa_mask <- terra::mask(esalandcover, x_v)
    arearaster <- cellSize(esa_mask, mask = TRUE, unit = "ha")
    total_size <- as.numeric(global(arearaster, fun = sum, na.rm = TRUE))

    purrr::map_dfr(1:nlyr(esa_mask), function(i) {
      zonal(arearaster, esa_mask[[i]], sum) %>%
        tidyr::pivot_longer(cols = -area, names_to = "datetime", values_to = "code") %>%
        dplyr::left_join(.esa_landcover_classes, by = "code") %>%
        dplyr::mutate(
          datetime = regmatches(datetime, regexpr("\\d{4}", datetime)),
          datetime = as.POSIXct(paste0(datetime, "-01-01T00:00:00Z")),
          variable = variable,
          unit = "ha"
        ) %>%
        dplyr::select(datetime, variable, unit, value = area)
    })
  }
}

.esa_landcover_classes <- data.frame(
  code = c(0, 111:116, 121:126, seq(20, 100, 10), 200),
  variable = c(
    "no_data", "closed_forest_evergreen_needle_leaf", "closed_forest_evergreen_broad_leaf", "closed_forest_deciduous_needle_leaf",
    "closed_forest_deciduous_broad_leaf", "closed_forest_mixed", "closed_forest_unknown", "open_forest_evergreen_needle_leaf",
    "open_forest_evergreen_broad_leaf", "open_forest_deciduous_needle_leaf", "open_forest_deciduous_broad_leaf",
    "open_forest_mixed", "open_forest_unknown", "shrubs", "herbaceous_vegetation", "cropland", "built_up", "bare_vegetation",
    "snow_and_ice", "permanent_water_bodies", "herbaceous_wetland", "moss_and_lichen", "open_sea"
  )
)

register_indicator(
  name = "landcover",
  description = "Areal statistics grouped by landcover class",
  resources = "esalandcover"
)
