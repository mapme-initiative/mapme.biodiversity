#' Calculate biomes statistics (TEOW) based on WWF
#'
#' This function allows to efficiently retrieve the name of the biomes
#' and compute the corresponding area from Terrestrial Ecoregions of the
#' World (TEOW) - World Wildlife Fund (WWF) for polygons. For each polygon,
#' the name and area of the biomes (in hectare) is returned.
#' The required resources for this indicator are:
#'  - [ecoregions]
#'
#' @name biome
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for name of the biomes and corresponding area (in ha).
#' @examples
#' library(sf)
#' library(mapme.biodiversity)
#'
#' temp_loc <- file.path(tempdir(), "mapme.biodiversity")
#' if(!file.exists(temp_loc)){
#' dir.create(temp_loc)
#' resource_dir <- system.file("res", package = "mapme.biodiversity")
#' file.copy(resource_dir, temp_loc, recursive = TRUE)
#' }
#'
#' (try(aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'                         package = "mapme.biodiversity") %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2001,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("ecoregions") %>%
#'   calc_indicators("biome") %>%
#'   tidyr::unnest(biome)))
NULL

#' Calculate biomes statistics (TEOW) based on WWF
#'
#' Considering global TEOW polygons from WWF for the year 2001 users can
#' retrieve the name of the biomes and compute the corresponding area
#' of the particular biomes for their polygons.
#'
#' @param shp A single polygon for which to calculate the biomes statistic
#' @param ecoregions The ecoregions vector resource (TEOW - WWF)
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary vector files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @noRd


.calc_biome <- function(shp,
                        ecoregions,
                        rundir = tempdir(),
                        verbose = TRUE,
                        todisk = FALSE,
                        ...) {
  BIOME_NAME <- NULL
  biomes <- NULL
  new_area <- NULL
  if(nrow(ecoregions[[1]]) == 0) return(NA)

  merged <- .comp_teow(
    shp = shp,
    ecoregions = ecoregions,
    rundir = rundir,
    verbose = verbose,
    todisk = todisk
  )
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
