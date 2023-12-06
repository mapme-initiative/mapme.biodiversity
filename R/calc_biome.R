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
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for name of the biomes and corresponding area (in ha).
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
#'     years = 2001,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("teow") %>%
#'   calc_indicators("biome") %>%
#'   tidyr::unnest(biome)
#'
#' aoi
#' }
NULL

#' Calculate biomes statistics (TEOW) based on WWF
#'
#' Considering global TEOW polygons from WWF for the year 2001 users can
#' retrieve the name of the biomes and compute the corresponding area
#' of the particular biomes for their polygons.
#'
#' @param x A single polygon for which to calculate the biomes statistic
#' @param teow The teow vector resource (TEOW - WWF)
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_biome <- function(x,
                        teow,
                        verbose = TRUE,
                        ...) {
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
    verbose = verbose)

  if (nrow(merged) == 0) {
    return(NA)
  }

  out <- merged %>%
    dplyr::select(BIOME_NAME, new_area)

  out_tibble <- tibble(
    biomes = out[[1]],
    area = out[[2]])

  results_biome <- out_tibble %>%
    dplyr::group_by(biomes) %>%
    dplyr::summarise(area = sum(as.numeric(area)))

  results_biome
}

register_indicator(
  name = "biome",
  resources = list(teow = "vector"),
  fun = .calc_biome,
  arguments = list(),
  processing_mode = "asset"
)
