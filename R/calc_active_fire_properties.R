#' Calculate active fire properties based on NASA FIRMS polygons
#'
#' This function allows to efficiently extract the properties of fire
#' events occurred in the region of interest from the NASA FIRMS active
#' fire polygon datasets. For each polygon, the fire events properties
#' like fire pixel brightness temperature, and fire radiative power (frp)
#' along with fire hotspots for the desired year is returned.
#' The required resources for this indicator are:
#'  - [nasa_firms]
#'
#' @name active_fire_properties
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for the 15 different fire events variables
#'   including lon/lat coordinates.
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
#'     years = 2021,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("nasa_firms", instrument = "VIIRS") %>%
#'   calc_indicators("active_fire_properties") %>%
#'   tidyr::unnest(active_fire_properties)
#'
#' aoi
#' }
NULL

#' Calculate active fire properties based on FIRMS
#'
#' Considering FIRMS polygons from NASA users can extract the
#' properties of fire events occurred in the region of interest for
#' years 2000-2021 (MODIS) and 2012-2021 (VIIRS).
#'
#' @param x A single polygon for which to calculate the active fire properties
#' @param nasa_firms The active fire vector resource (NASA - FIRMS)
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_active_fire_properties <- function(x,
                                         nasa_firms,
                                         verbose = TRUE,
                                         ...) {
  # change quality flag to charachter to allow binding MODIS and VIIRS
  nasa_firms <- lapply(nasa_firms, function(x) {
    x$confidence <- as.character(x$confidence)
    x
  })
  # row bind the frames
  nasa_firms <- dplyr::bind_rows(nasa_firms)
  intersected <- suppressWarnings(st_intersection(nasa_firms, st_geometry(x)))
  if (nrow(intersected) == 0) {
    return(NA)
  }
  coordinates <- st_coordinates(intersected)
  intersected <- dplyr::as_tibble(intersected)
  intersected <- dplyr::select(intersected, -geom)
  dplyr::mutate(
    intersected,
    longitude = coordinates[, 1, drop = TRUE],
    latitude = coordinates[, 2, drop = TRUE]
  )
}

register_indicator(
  name = "active_fire_properties",
  resources = list(nasa_firms = "vector"),
  fun = .calc_active_fire_properties,
  arguments = list(),
  processing_mode = "asset"
)
