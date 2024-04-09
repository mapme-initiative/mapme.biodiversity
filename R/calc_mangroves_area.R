#' Calculate mangrove extent based on Global Mangrove Watch (GMW)
#'
#' This function allows to efficiently calculate area of mangrove from
#' Global Mangrove Watch - World Conservation Monitoring Centre (WCMC)
#' for polygons. For each polygon, the area of the mangrove (in hectare)
#' for desired year is returned.
#'
#' The required resources for this indicator are:
#'  - [gmw]
#'
#' @name mangroves_area
#' @docType data
#' @keywords indicator
#' @returns A function that returns a tibble with a column for area of mangrove
#'   (in ha) and corresponding year.
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
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_gmw(years = c(1996, 2016))) %>%
#'   calc_indicators(calc_mangroves_area()) %>%
#'   tidyr::unnest(mangroves_area)
#'
#' aoi
#' }
calc_mangroves_area <- function() {
  function(x = NULL,
           gmw = NULL,
           name = "mangroves_area",
           mode = "asset",
           verbose = mapme_options()[["verbose"]]) {
    results <- lapply(1:length(gmw), function(j) {
      intersected <- suppressWarnings(st_intersection(gmw[[j]], x))
      area <- st_area(intersected) %>%
        as.numeric() %>%
        sum() %>%
        `/`(., 10000)
      out <- tibble(
        mangrove_extent = area,
        year = strsplit(names(gmw[j]), "_|.gpkg")[[1]][2]
      )
    })
    results <- tibble(do.call(rbind, results))
    results
  }
}

register_indicator(
  name = "mangroves_area",
  description = "Area covered by mangroves",
  resources = "gmw"
)
