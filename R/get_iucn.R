#' IUCN Red List of Threatened Species
#'
#' This resource is part of the spatial data set Red List of Threatened Species
#' released by IUCN. It is free to use under a non-commercial licence. For
#' commercial uses, a request has to be sent to Integrated Biodiversity
#' Assessment Tool (IBAT).
#'
#' To use this data in mapme workflows, you will have to manually download the
#' global data set and point towards the file path on your local machine.
#' Please find the available data under the source link given below.
#'
#' @name iucn
#' @param paths A character vector to the respective species range files in GTiff
#'   format. Note, that theses files have to be downloaded manually.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references IUCN (2024). The IUCN Red List of Threatened Species.
#'   \url{https://www.iucnredlist.org}
#' @source \url{https://www.iucnredlist.org/resources/other-spatial-downloads}
#' @include register.R
#' @export
get_iucn <- function(paths = NULL) {
  if (is.null(paths) || !all(endsWith(paths, ".tif")) || !all(sapply(paths, spds_exists, what = "raster"))) {
    stop("Expecting paths to point towards existing GTiff files.")
  }

  if (!all(grepl("_SR_", basename(paths)))) {
    stop("Filenames do match expected schema for IUCN species richness rasters.")
  }

  function(
      x,
      name = "iucn",
      type = "raster",
      outdir = mapme_options()[["outdir"]],
      verbose = mapme_options()[["verbose"]]) {
    make_footprints(
      paths,
      what = "raster",
      co = c("-co", "INTERLEAVE=BAND", "-co", "COMPRESS=LZW", "-ot", "Int16")
    )
  }
}

register_resource(
  name = "iucn",
  description = "IUCN Species Richness Raser Dataset",
  licence = "https://www.iucnredlist.org/terms/terms-of-use",
  source = "https://www.iucnredlist.org/resources/other-spatial-downloads",
  type = "raster"
)
