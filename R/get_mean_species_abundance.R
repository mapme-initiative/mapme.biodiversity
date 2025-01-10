#' Mean Species Abundance
#'
#' The variable represents the mean species abundance (MSA), for plants and
#' warmblooded vertabrates: values ranging from 0 to 1 indicating local
#' biodiversity intactness, relative to a pristine reference situation.
#'
#' To use this data in mapme workflows, you will have to manually download the
#' global data set and point towards the file path on your local machine.
#' Please find the available data under the source link given below.
#'
#' @name mean_species_abundance_resource
#' @keywords resource
#' @param path A character vector to the msa data file
#' @returns A function that returns an `sf` footprints object.
#' @references Schipper, AM, Hilbers, JP, Meijer, JR, et al. Projecting
#'   terrestrial biodiversity intactness with GLOBIO 4. Glob Change Biol. 2020;
#'   26: 760– 771.
#'   \doi{https://doi.org/10.1111/gcb.14848}
#' @source \url{https://www.globio.info/globio-data-downloads}
#' @include register.R
#' @export
get_mean_species_abundance <- function(path = NULL) {
  if (is.null(path) || !endsWith(path, ".tif") || !spds_exists(path, what = "raster")) {
    stop("Expecting path to point towards an existing '.tif' file.")
  }

  function(
    x,
    name = "mean_species_abundance",
    type = "raster",
    outdir = mapme_options()[["outdir"]],
    verbose = mapme_options()[["verbose"]]) {
    filename <- basename(path)
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
  name = "mean_species_abundance",
  description = "Mean Species Abundance",
  licence = "CC-BY-4.0",
  source = "https://www.globio.info/globio-data-downloads",
  type = "raster"
)
