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
#' @keywords indicator
#' @returns A function that returns a tibble with a column for the 15 different
#'   fire events variables including lon/lat coordinates.
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
#'   get_resources(get_nasa_firms(years = 2021, instrument = "VIIRS")) %>%
#'   calc_indicators(calc_active_fire_properties()) %>%
#'   tidyr::unnest(active_fire_properties)
#'
#' aoi
#' }
calc_active_fire_properties <- function() {
  function(x,
           nasa_firms = NULL,
           name = "active_fire_properties",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
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
}

register_indicator(
  name = "active_fire_properties",
  description = "Extraction of properties of fires detected by NASA FIRMS",
  resources = "nasa_firms"
)
