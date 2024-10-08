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
#' @returns A function that returns an indicator tibble with mangroves as variable
#'   and corresponding areas (in ha) as value.
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
#'   portfolio_long()
#'
#' aoi
#' }
calc_mangroves_area <- function() {
  function(x = NULL,
           gmw = NULL,
           name = "mangroves_area",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(gmw)) {
      return(NULL)
    }

    results <- purrr::map(1:length(gmw), function(j) {
      mangroves <- st_make_valid(gmw[[j]])
      intersected <- suppressWarnings(st_intersection(mangroves, x))
      intersected <- st_make_valid(intersected)
      intersected <- intersected[st_is_valid(intersected), ]
      area <- sum(as.numeric(st_area(intersected)), na.rm = TRUE) / 10000
      year <- strsplit(names(gmw[j]), "_|.gpkg")[[1]][3]

      tibble::tibble(
        datetime = as.POSIXct(paste0(year, "-01-01T00:00:00Z")),
        variable = "mangroves",
        unit = "ha",
        value = area
      )
    }) %>%
      purrr::list_rbind()
  }
}

register_indicator(
  name = "mangroves_area",
  description = "Area covered by mangroves",
  resources = "gmw"
)
