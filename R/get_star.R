#' Species Threat Abatement and Restoration (STAR) layers
#'
#' STAR layers are calculated based on the current and hypothetical area of
#' habitat (AOH) for species considered under threat by the IUCN Red List.
#' The version used here is the 50 km resolution published with the original
#' research paper. Thus it should only be used for continental/global scale
#' analysis.
#'
#' The threat abatement layer is calculated for each resolution cell by considering
#' the current AOH for each species weighted by its threat level (Near Threatened = 1;
#' Vulnerable = 2; Endangered = 3; Critically Endangered = 4) and the
#' relative contribution of different thread categories on that species.
#'
#' The restoration layer considers the extent of restorable AOH at each resolution
#' cell weighted by species threat level (see above), threat category, and
#' additionally weighted by a global multiplier to discount restoration scores.
#'
#' Consider the methods section of the original publication for additional
#' information.
#'
#' @name star
#' @param paths A character vector to the respective species range files in GTiff
#'   format. Note, that theses files have to be downloaded manually.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Mair, L., Bennun, L.A., Brooks, T.M. et al. A metric for
#'   spatially explicit contributions to science-based species targets.
#'   Nat Ecol Evol 5, 836–844 (2021). \doi{https://doi.org/10.1038/s41559-021-01432-0}
#' @source \url{https://www.nature.com/articles/s41559-021-01432-0}
#' @include register.R
#' @export
get_star <- function(layer = c("both", "abatement", "restoration")) {
  layer <- match.arg(layer)
  if (layer == "both") layer <- c("abatement", "restoration")

  function(x,
           name = "star",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    layers <- c(
      abatement = "/vsicurl/https://static-content.springer.com/esm/art%3A10.1038%2Fs41559-021-01432-0/MediaObjects/41559_2021_1432_MOESM4_ESM.tif",
      restoration = "/vsicurl/https://static-content.springer.com/esm/art%3A10.1038%2Fs41559-021-01432-0/MediaObjects/41559_2021_1432_MOESM5_ESM.tif"
    )

    if (length(layer) == 1) {
      if (layer == "abatement") layers <- layers["abatement"]
      if (layer == "restoration") layers <- layers["restoration"]
    }

    filenames <- paste0(names(layers), ".tif")
    if (!is.null(outdir)) {
      if (all(sapply(file.path(outdir, filenames), spds_exists))) {
        layers <- file.path(outdir, filenames)
      }
    }

    make_footprints(
      layers,
      filenames,
      what = "raster",
      co = c("-co", "INTERLEAVE=BAND", "-co", "COMPRESS=LZW", "-ot", "Float32")
    )
  }
}

register_resource(
  name = "star",
  description = "Species Threat Abatement and Restoration (STAR) layers",
  licence = "https://www.ibat-alliance.org/terms",
  source = "https://www.nature.com/articles/s41559-021-01432-0#data-availability",
  type = "raster"
)
