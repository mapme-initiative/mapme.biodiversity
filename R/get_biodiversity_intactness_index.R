#' Biodiversity Intactness Index
#'
#' The variable is the modelled average abundance of originally-present species,
#' relative to their abundance in an intact ecosystem. Please refer to
#' Newbold et al. (2016) for all details, and please cite it when using these
#' data.
#'
#' To use this data in mapme workflows, you will have to manually download the
#' global data set and point towards the file path on your local machine.
#' Please find the available data under the source link given below.
#'
#' @name biodiversity_intactness_index_resource
#' @keywords resource
#' @param path A character vector to the biodiversity intactness index ASCII file.
#' @returns A function that returns an `sf` footprints object.
#' @references Tim Newbold; Lawrence Hudson; Andy Arnell; Sara Contu et al.
#'   (2016). Global map of the Biodiversity Intactness Index, from Newbold et al.
#'   (2016) Science \[Data set\]. Natural History Museum.
#'   \doi{https://doi.org/10.5519/0009936}
#' @include register.R
#' @export
get_biodiversity_intactness_index <- function(path = NULL) {
  if (is.null(path) || !endsWith(path, ".asc") || !spds_exists(path, what = "raster")) {
    stop("Expecting path to point towards an existing '.asc' file.")
  }

  function(
      x,
      name = "biodiversity_intactness_index",
      type = "raster",
      outdir = mapme_options()[["outdir"]],
      verbose = mapme_options()[["verbose"]]) {
    filename <- gsub("asc", "tif", basename(path))
    fps <- make_footprints(
      path,
      filename,
      what = "raster",
      co = c("-co", "INTERLEAVE=BAND", "-co", "COMPRESS=LZW", "-ot", "Float32")
    )
    st_crs(fps) <- st_crs("EPSG:4326")
    fps
  }
}

register_resource(
  name = "biodiversity_intactness_index",
  description = "Biodiversity Intactness Index",
  licence = "CC-BY-4.0",
  source = "https://data.nhm.ac.uk/dataset/global-map-of-the-biodiversity-intactness-index-from-newbold-et-al-2016-science",
  type = "raster"
)
