#' Calculate mangrove extent based on Global Mangrove Watch (GMW)
#'
#' This function allows to efficiently calculate area of mangrove from
#' Global Mangrove Watch - World Conservation Monitoring Centre (WCMC)
#' for polygons. For each polygon, the area of the mangrove (in hectare)
#' for desired year is returned.
#' The required resources for this indicator are:
#'  - [mangrove]
#'
#' @name gmw
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for area of mangrove (in ha) and corresponding year.
#' @examples
#' library(sf)
#' library(mapme.biodiversity)
#' (aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = c(1996, 2016),
#'     outdir = system.file("res", package = "mapme.biodiversity"),
#'     tmpdir = system.file("tmp", package = "mapme.biodiversity"),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("mangrove") %>%
#'   calc_indicators("gmw") %>%
#'   tidyr::unnest(gmw))
NULL

#' Calculate mangrove extent based on Global Mangrove Watch (GMW)
#'
#' Considering 25 meter spatial resolution global polygons users can compute
#' the mangrve extent area for their area of interest available from 1996 to
#' 2016 with periodic updates in between.
#'
#' @param shp A single polygon for which to calculate the mangrove extent
#' @param mangrove The mangrove vector resource (GMW)
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary vector files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @noRd

.calc_gmw <- function(shp,
                      mangrove,
                      rundir = tempdir(),
                      verbose = TRUE,
                      todisk = FALSE,
                      ...) {
  results <- lapply(1:length(mangrove), function(j) {
    intersected <- suppressWarnings(st_intersection(mangrove[[j]], shp))
    area <- st_area(intersected) %>%
      as.numeric() %>%
      sum() %>%
      `/`(., 10000)
    out <- tibble(
      mangrove_extent = area,
      year = strsplit(names(mangrove[j]), "_|.gpkg")[[1]][2]
    )
  })
  results <- tibble(do.call(rbind, results))
  results
}
