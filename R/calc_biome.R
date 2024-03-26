#' Calculate biomes statistics (TEOW) based on WWF
#'
#' This function allows to efficiently retrieve the name of the biomes
#' and compute the corresponding area from Terrestrial Ecoregions of the
#' World (TEOW) - World Wildlife Fund (WWF) for polygons. For each polygon,
#' the name and area of the biomes (in hectare) is returned.
#' The required resources for this indicator are:
#'  - [teow]
#'
#' @name biome
#' @keywords indicator
#' @returns A function that returns a tibble with a column for name of the
#'   biomes and corresponding area (in ha).
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
#'   calc_indicators(calc_biome()) %>%
#'   tidyr::unnest(biome)
#'
#' aoi
#' }
calc_biome <- function() {
  function(x,
           teow = NULL,
           name = "biome",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
    BIOME_NAME <- NULL
    biomes <- NULL
    new_area <- NULL
    area <- NULL

    if (nrow(teow[[1]]) == 0) {
      return(NA)
    }

    merged <- .comp_teow(
      x = x,
      teow = teow,
      verbose = verbose
    )

    if (nrow(merged) == 0) {
      return(NA)
    }

    out <- merged %>%
      dplyr::select(BIOME_NAME, new_area)

    out_tibble <- tibble(
      biomes = out[[1]],
      area = out[[2]]
    )

    results_biome <- out_tibble %>%
      dplyr::group_by(biomes) %>%
      dplyr::summarise(area = sum(as.numeric(area)))

    results_biome
  }
}

register_indicator(
  name = "biome",
  description = "Areal statistics of biomes from TEOW",
  resources = "teow"
)
