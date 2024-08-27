#' Calculate Key Biodiversity Areas
#'
#' This function calculates the total area of key biodiversity areas for a given
#' input polygon.
#'
#' The required resources for this indicator are:
#'  - [key_biodiversity_areas_resource]
#'
#' @name key_biodiversity_areas_indicator
#' @docType data
#' @keywords indicator
#' @format A function returning an indicator tibble with `key_biodiversity_area`
#'   as variable and the total overlap area (in ha) as value.
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
#' aoi <- read_sf(
#'   system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'               package = "mapme.biodiversity"
#' ))
#' kbas <- system.file("res", "key_biodiversity_areas", "kbas.gpkg",
#'                     package = "mapme.biodiversity")
#' aoi <- get_resources(aoi, get_key_biodiversity_areas(kbas))
#' aoi <- calc_indicators(aoi, calc_key_biodiversity_area())
#' aoi <- portfolio_long(aoi)
#'
#' aoi
#' }
calc_key_biodiversity_area <- function() {
  function(x = NULL,
           key_biodiversity_areas,
           name = "key_biodiversity_areas",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {

    key_biodiversity_areas <- key_biodiversity_areas[[1]]
    if (is.null(key_biodiversity_areas)) {
      return(NULL)
    }
    if (length(key_biodiversity_areas) == 0) {
      return(NULL)
    }

    int_area <- suppressWarnings(st_intersection(x, key_biodiversity_areas))
    if (nrow(int_area) == 0) return(NULL)
    int_area_ha <- as.numeric(sum(st_area(int_area), na.rm = TRUE) / 10000)

    results <- tibble::tibble(
      datetime = as.POSIXct("2024-01-01T00:00:00Z"),
      variable = "key_biodiversity_area",
      unit = "ha",
      value = int_area_ha
    )

    return(results)
  }
}

register_indicator(
  name = "key_biodiversity_areas",
  description = "Area estimation of intersection with key biodiversity areas.",
  resources = "key_biodiversity_areas"
)
