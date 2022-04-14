#' Calculate terrestrial ecoregions statistics (TEOW) based on WWF
#'
#' This function allows to efficiently retrieve the name of the ecoregions
#' and compute the corresponding area from Terrestrial Ecoregions of the
#' World (TEOW) - World Wildlife Fund (WWF) for polygons. For each polygon,
#' the name and area of the ecoregions (in hectare) is returned.
#' The required resources for this indicator are:
#'  - [ecoregions]
#'
#' @name teow
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for name of the ecoregions and corresponding area (in ha).
#' @examples
#' library(sf)
#' library(mapme.biodiversity)
#' (aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg", package = "mapme.biodiversity") %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2001,
#'     outdir = system.file("res", package = "mapme.biodiversity"),
#'     tmpdir = system.file("tmp", package = "mapme.biodiversity"),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("ecoregions") %>%
#'   calc_indicators("teow") %>%
#'   tidyr::unnest(teow))
NULL

#' Calculate terrestrial ecoregions statistics (TEOW) based on WWF
#'
#' Considering global TEOW polygons from WWF for the year 2001 users can
#' retrieve the name of the ecoregions and compute the corresponding area
#' of the particular ecoregions for their polygons.
#'
#' @param shp A single polygon for which to calculate the ecoregion statistics
#' @param ecoregions The ecoregions vector resource (TEOW - WWF)
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary vector files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @noRd

.calc_teow <- function(shp,
                       ecoregions,
                       rundir = tempdir(),
                       verbose = TRUE,
                       todisk = FALSE,
                       ...) {
  ECO_NAME <- NULL
  new_area <- NULL
  if (nrow(ecoregions[[1]]) == 0) {
    return(NA)
  }
  merged <- .comp_teow(
    shp = shp,
    ecoregions = ecoregions,
    rundir = rundir,
    verbose = verbose,
    todisk = todisk
  )
  out <- merged %>%
    dplyr::select(ECO_NAME, new_area)
  out_tibble <- tibble(
    ecoregions = out[[1]],
    area = out[[2]]
  )
  out_tibble %>%
    dplyr::group_by(ecoregions) %>%
    dplyr::summarise(area = sum(as.numeric(area)))
}

#' Helper function to intersect polygons and add biome names
#'
#' @param ecoregions terrestrial ecoregions vector from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_teow <- function(shp,
                       ecoregions,
                       rundir = tempdir(),
                       verbose = TRUE,
                       todisk = FALSE,
                       ...) {
  intersected <- suppressWarnings(st_intersection(shp, ecoregions[[1]]))
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
  merged <- merge(intersected, biome_and_name)
  area <- st_area(merged) %>%
    as.numeric() %>%
    `/`(., 10000)
  merged <- st_drop_geometry(merged)
  merged$new_area <- area
  return(merged)
}
