#' Calculate active fire counts based on NASA FIRMS polygons
#'
#' This function allows to efficiently calculate the number of fire
#' events occurred in the region of interest from the NASA FIRMS active
#' fire polygon datasets. For each polygon, the fire event counts for
#' the desired year is returned.
#' The required resources for this indicator are:
#'  - [nasa_firms]
#'
#' @name active_fire_counts
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for number of fire events and corresponding year.
#' @examples
#' library(sf)
#' library(mapme.biodiversity)
#'
#' temp_loc <- file.path(tempdir(), "mapme.biodiversity")
#' if (!file.exists(temp_loc)) {
#'   dir.create(temp_loc)
#'   resource_dir <- system.file("res", package = "mapme.biodiversity")
#'   file.copy(resource_dir, temp_loc, recursive = TRUE)
#' }
#'
#' (try(aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2021,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("nasa_firms") %>%
#'   calc_indicators("active_fire_counts") %>%
#'   tidyr::unnest(active_fire_counts)))
NULL

#' Calculate active fire counts based on FIRMS
#'
#' Considering FIRMS polygons from NASA users can compute the
#' number of fire events occured in the region of interest for
#' years 2000-2021 (MODIS) and 2012-2021 (VIIRS).
#'
#' @param shp A single polygon for which to calculate the active fire counts
#' @param nasa_firms The active fire vector resource (NASA - FIRMS)
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary vector files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @noRd

.calc_active_fire_counts <- function(shp,
                                     nasa_firms,
                                     rundir = tempdir(),
                                     verbose = TRUE,
                                     todisk = FALSE,
                                     ...) {
  acq_date <- NULL
  intersected <- suppressWarnings(st_intersection(nasa_firms, shp))
  if(nrow(intersected) == 0) return(NA)
  intersected <-  tidyr::separate(intersected, acq_date, c("yyyy", "mm", "dd"))
  years <- unique(intersected$yyyy)
  data <- lapply(1:length(years), function(i) {
    n <- nrow(intersected[intersected$yyyy == paste0(years[i]), ])
    df <- data.frame(
      year = years[i],
      active_fire_counts = n
    )
    df
  })
  results <- do.call(rbind, data)
  results
}
