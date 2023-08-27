#' Calculate deforestation drivers
#'
#' This function extracts areal statistics for the drivers of deforestation
#' based on the data source produced by Fritz et al (2022). The required
#' resource is:
#'  - [fritz_et_al]
#'
#' @name deforestation_drivers
#' @docType data
#' @keywords indicator
#' @format A tibble with 3 columns indicating the class of a deforestation driver,
#'   the absolute area in ha, and the percentage in relation to the total
#'   area of forest loss as indicated by the Fritz et al. (2022) resource.
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
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2021,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("fritz_et_al", res_drivers = 100) %>%
#'   calc_indicators("deforestation_drivers") %>%
#'   tidyr::unnest(deforestation_drivers)
#'
#' aoi
#' }
NULL


#' @include register.R
#' @noRd
.calc_deforestation_drivers <- function(x,
                                        fritz_et_al,
                                        verbose = TRUE,
                                        ...) {
  if (is.null(fritz_et_al)) {
    return(NA)
  }
  classes <- data.frame(
    class = c(
      "commercial agriculture", "commercial oil palm",
      "managed forests", "mining", "natural disturbances", "pasture",
      "roads", "wildfire", "other subsistance agriculture",
      "shifting cultivation"
    ),
    code = c(1:7, 9, 80, 81)
  )
  cropped <- mask(fritz_et_al, x)
  names(cropped) <- "code"
  arearaster <- cellSize(cropped, unit = "ha")
  arearaster <- mask(arearaster, x)
  zonal_stat <- zonal(arearaster, cropped, fun = "sum")

  dplyr::left_join(classes, zonal_stat, by = "code") %>%
    tidyr::replace_na(list(area = 0)) %>%
    dplyr::select(class, area) %>%
    dplyr::mutate(percent = area / sum(area)) %>%
    tidyr::replace_na(list(percent = 0)) %>%
    tibble::as_tibble()
}

register_indicator(
  name = "deforestation_drivers",
  resources = list(fritz_et_al = "raster"),
  fun = .calc_deforestation_drivers,
  arguments = list(),
  processing_mode = "asset"
)
