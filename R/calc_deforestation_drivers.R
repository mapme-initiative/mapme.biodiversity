#' Calculate deforestation drivers
#'
#' This function extracts areal statistics for the drivers of deforestation
#' based on the data source produced by Fritz et al (2022).
#'
#' The required resource for this indicator is:
#'  - [fritz_et_al]
#'
#' @name deforestation_drivers
#' @keywords indicator
#' @returns A function that returns a tibble with 3 columns indicating the
#'   class of a deforestation driver, the absolute area in ha, and the
#'   percentage in relation to the total area of forest loss as indicated by
#'   the Fritz et al. (2022) resource.
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
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_fritz_et_al(resolution = 100)) %>%
#'   calc_indicators(calc_deforestation_drivers()) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_deforestation_drivers <- function() {
  function(x,
           fritz_et_al = NULL,
           name = "deforestation_drivers",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(fritz_et_al)) {
      return(NULL)
    }
    classes <- data.frame(
      class = c(
        "commercial_agriculture", "commercial_oil_palm",
        "managed_forests", "mining", "natural_disturbances", "pasture",
        "roads", "wildfire", "other_subsistance_agriculture",
        "shifting_cultivation"
      ),
      code = c(1:7, 9, 80, 81)
    )

    fritz_et_al <- terra::mask(fritz_et_al, x)
    names(fritz_et_al) <- "code"
    arearaster <- cellSize(fritz_et_al, unit = "ha")
    zonal_stat <- zonal(arearaster, fritz_et_al, fun = "sum")

    dplyr::left_join(classes, zonal_stat, by = "code") %>%
      tidyr::replace_na(list(area = 0)) %>%
      dplyr::mutate(
        datetime = "2008-01-01",
        variable = class,
        unit = "ha",
        value = area
      ) %>%
      dplyr::select(datetime, variable, unit, value) %>%
      tibble::as_tibble()
  }
}

register_indicator(
  name = "deforestation_drivers",
  description = "Areal statistics of deforestation drivers",
  resources = "fritz_et_al"
)
