#' Calculate mangrove extent based on Global Mangrove Watch (GMW)
#'
#' This function allows to efficiently calculate area of mangrove from
#' Global Mangrove Watch - World Conservation Monitoring Centre (WCMC)
#' for polygons. For each polygon, the area of the mangrove (in hectare)
#' for desired year is returned.
#' The required resources for this indicator are:
#'  - [gmw]
#'
#' @name mangroves_area
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for area of mangrove (in ha) and corresponding year.
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
#'     years = c(1996, 2016),
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("gmw") %>%
#'   calc_indicators("mangroves_area") %>%
#'   tidyr::unnest(mangroves_area)
#'
#' aoi
#' }
NULL

#' Calculate mangrove extent based on Global Mangrove Watch (GMW)
#'
#' Considering 25 meter spatial resolution global polygons users can compute
#' the mangrve extent area for their area of interest available from 1996 to
#' 2020 with periodic updates in between.
#'
#' @param x A single polygon for which to calculate the mangrove extent
#' @param gmw The mangrove vector resource (GMW)
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_mangroves_area <- function(x,
                                 gmw,
                                 verbose = TRUE,
                                 ...) {
  results <- lapply(1:length(gmw), function(j) {
    intersected <- suppressWarnings(st_intersection(gmw[[j]], x))
    area <- st_area(intersected) %>%
      as.numeric() %>%
      sum() %>%
      `/`(., 10000)
    out <- tibble(
      mangrove_extent = area,
      year = strsplit(names(gmw[j]), "_|.gpkg")[[1]][2]
    )
  })
  results <- tibble(do.call(rbind, results))
  results
}

register_indicator(
  name = "mangroves_area",
  resources = list(gmw = "vector"),
  fun = .calc_mangroves_area,
  arguments = list(),
  processing_mode = "asset"
)
