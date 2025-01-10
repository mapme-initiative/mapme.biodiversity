#' Calculate Mean Species Abundance
#'
#' This function calculates the average of the mean species abundance index for
#' a region.
#'
#' The required resources for this indicator are:
#'  - [mean_species_abundance]
#'
#' @name mean_species_abundance_indicator
#' @docType data
#' @keywords indicator
#' @format  A function that returns an indicator tibble with variable
#'   mean_species_abundance and corresponding values (unitless) as value.
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
#' msa <- system.file("res", "mean_species_abundance",
#'                    "TerrestrialMSA_2015_World.tif",
#'                    package = "mapme.biodiversity")
#'
#' aoi <- read_sf(
#'   system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'               package = "mapme.biodiversity"
#' ))
#' aoi <- get_resources(aoi, get_mean_species_abundance(msa))
#' aoi <- calc_indicators(aoi, calc_mean_species_abundance())
#' aoi <- portfolio_long(aoi)
#'
#' aoi
#' }
calc_mean_species_abundance <- function() {

  function(x = NULL,
           mean_species_abundance,
           name = "mean_species_abundance",
           mode = "asset",
           aggregation = "mean",
           verbose = mapme_options()[["verbose"]]) {

    if (is.null(mean_species_abundance)) {
      return(NULL)
    }

    mean_msa <- exactextractr::exact_extract(mean_species_abundance, x, fun = "mean")

    results <- tibble::tibble(
      datetime = as.POSIXct("2015-01-01T00:00:00Z"),
      variable = "mean_species_abundance",
      unit = "unitless",
      value = mean_msa
    )

    return(results)
  }
}

register_indicator(
  name = "mean_species_abundance",
  description = "Averaged mean species abundance.",
  resources = "mean_species_abundance"
)
