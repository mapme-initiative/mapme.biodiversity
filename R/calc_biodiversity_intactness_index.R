#' Calculate Biodiversity Intactness Index
#'
#' This function calculates the mean biodiversity intactness index for a region.
#'
#' The required resources for this indicator are:
#'  - [biodiversity_intactness_index_resource]
#'
#' @name biodiversity_intactness_index_indicator
#' @docType data
#' @keywords indicator
#' @format  A function that returns an indicator tibble with variable
#'   biodiversity_intactness_index and corresponding values (unitless) as value.
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
#' lbii <- system.file("res", "biodiversity_intactness_index", "lbii.asc",
#'                     package = "mapme.biodiversity")
#'
#' aoi <- read_sf(
#'   system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'               package = "mapme.biodiversity"
#' ))
#' aoi <- get_resources(aoi, get_biodiversity_intactness_index(lbii))
#' aoi <- calc_indicators(aoi, calc_biodiversity_intactness_index())
#' aoi <- portfolio_long(aoi)
#'
#' aoi
#' }
calc_biodiversity_intactness_index <- function() {

  function(x = NULL,
           biodiversity_intactness_index,
           name = "biodiversity_intactness_index",
           mode = "asset",
           aggregation = "mean",
           verbose = mapme_options()[["verbose"]]) {

    if (is.null(biodiversity_intactness_index)) {
      return(NULL)
    }

    mean_bii <- exactextractr::exact_extract(biodiversity_intactness_index, x, fun = "mean")

    results <- tibble::tibble(
      datetime = as.POSIXct("2005-01-01T00:00:00Z"),
      variable = "biodiversity_intactness_index",
      unit = "unitless",
      value = mean_bii
    )

    return(results)
  }
}

register_indicator(
  name = "biodiversity_intactness_index",
  description = "Averaged biodiversity intactness index.",
  resources = "biodiversity_intactness_index"
)
