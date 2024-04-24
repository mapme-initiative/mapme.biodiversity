#' Calculate terrestrial ecoregions statistics (TEOW) based on WWF
#'
#' This function allows to efficiently retrieve the name of the ecoregions
#' and compute the corresponding area from Terrestrial Ecoregions of the
#' World (TEOW) - World Wildlife Fund (WWF) for polygons. For each polygon,
#' the name and area of the ecoregions (in hectare) is returned.
#' The required resources for this indicator are:
#'  - [teow]
#'
#' @name ecoregion
#' @keywords indicator
#' @returns A function that returns a tibble with a column for name of the
#'   ecoregions and corresponding area (in ha).
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
#'   get_resources(get_teow()) %>%
#'   calc_indicators(calc_ecoregion()) %>%
#'   tidyr::unnest(ecoregion)
#'
#' aoi
#' }
calc_ecoregion <- function() {
  function(x,
           teow = NULL,
           name = "ecoregion",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
    ECO_NAME <- NULL

    if (nrow(teow[[1]]) == 0) {
      return(NULL)
    }

    .comp_teow(
      x = x,
      teow = teow,
      var = ECO_NAME,
      verbose = verbose
    )
  }
}

#' Helper function to intersect polygons and add biome names
#'
#' @param teow terrestrial ecoregions vector from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_teow <- function(x,
                       teow,
                       var,
                       verbose = TRUE,
                       ...) {
  teow <- teow[[1]]
  teow <- st_make_valid(teow)

  intersected <- suppressWarnings(st_intersection(x, teow))
  biome_and_name <- data.frame(
    BIOME = c(1:14, 98, 99),
    BIOME_NAME = c(
      "Tropical & Subtropical Moist Broadleaf Forests",
      "Tropical & Subtropical Dry Broadleaf Forests",
      "Tropical & Subtropical Coniferous Forests",
      "Temperate Broadleaf & Mixed Forests",
      "Temperate Conifer Forests",
      "Boreal Forests/Taiga",
      "Tropical & Subtropical Grasslands, Savannas & Shrublands",
      "Temperate Grasslands, Savannas & Shrublands",
      "Flooded Grasslands & Savannas",
      "Montane Grasslands & Shrublands",
      "Tundra",
      "Mediterranean Forests, Woodlands & Scrub",
      "Deserts & Xeric Shrublands",
      "Mangroves",
      "Lake",
      "Rock and Ice"
    )
  )

  intersected <- merge(intersected, biome_and_name)
  intersected$value <- as.numeric((st_area(intersected))) / 10000

  if (nrow(intersected) == 0) {
    return(NULL)
  }

  intersected %>%
    sf::st_drop_geometry() %>%
    dplyr::select({{ var }}, value) %>%
    dplyr::mutate(
      datetime = as.Date("2000-01-01"),
      variable = {{ var }},
      unit = "ha"
    ) %>%
    dplyr::select(datetime, variable, unit, value) %>%
    dplyr::group_by(datetime, variable, unit) %>%
    dplyr::summarise(value = sum(as.numeric(value))) %>%
    dplyr::ungroup()
}

register_indicator(
  name = "ecoregion",
  description = "Areal statstics of ecoregions based on TEOW",
  resources = "teow"
)
