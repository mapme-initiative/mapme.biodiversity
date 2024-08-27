#' Key Biodiversity Areas
#'
#' This resource contains outlines of key biodiversity areas, which are areas
#' representing sites with specific importance for nature conservation.
#'
#' To use this data in mapme workflows, you will have to manually download the
#' global data set and point towards its file path on your local machine.
#' Please find the available data under the source link given below.
#'
#' @name key_biodiversity_areas_resource
#' @keywords resource
#' @param path A character vector to the key biodiversity areas GPKG file.
#'   Note, that the file has to be downloaded manually.
#' @returns A function that returns an `sf` footprints object.
#' @references BirdLife International (2024). The World Database of
#'   Key Biodiversity Areas. Developed by the KBA Partnership: BirdLife
#'   International, International Union for the Conservation of Nature,
#'   Amphibian Survival Alliance, Conservation International, Critical Ecosystem
#'   Partnership Fund, Global Environment Facility, Re:wild, NatureServe,
#'   Rainforest Trust, Royal Society for the Protection of Birds, Wildlife
#'   Conservation Society and World Wildlife Fund. Available at
#'   www.keybiodiversityareas.org.
#' @source \url{https://www.keybiodiversityareas.org/kba-data}
#' @include register.R
#' @export
get_key_biodiversity_areas <- function(path = NULL) {

  if(is.null(path) || !file.exists(path) || file.info(path) [["isdir"]]) {
    stop("Expecting path to point towards an existing file.")
  }

  function(
    x,
    name = "key_biodiversity_areas",
    type = "vector",
    outdir = mapme_options()[["outdir"]],
    verbose = mapme_options()[["verbose"]]) {

    bbox <- c(xmin = -180.0, ymin = -90.0, xmax = 180.0, ymax = 90.0)
    tile <- st_as_sf(st_as_sfc(st_bbox(bbox, crs = "EPSG:4326")))
    tile[["source"]] <- path
    make_footprints(tile, what = "vector")
  }
}

register_resource(
  name = "key_biodiversity_areas",
  description = "Key Biodiversity Areas",
  licence = "https://www.keybiodiversityareas.org/termsofservice",
  source = "https://www.keybiodiversityareas.org/kba-data",
  type = "vector"
)
