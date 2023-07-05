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
#' library(sf)
#' library(mapme.biodiversity)
#'
#' temp_loc <- file.path(tempdir(), "mapme.biodiversity")
#' if (!file.exists(temp_loc)) {
#'   dir.create(temp_loc)
#'   resource_dir <- system.file("res", package = "mapme.biodiversity")
#'   file.copy(resource_dir, temp_loc, recursive = TRUE)
#' }
#'
#' (try(aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2021,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("fritz_et_al", res_drivers = 100) %>%
#'   calc_indicators("deforestation_drivers") %>%
#'   tidyr::unnest(deforestation_drivers)))
NULL

.calc_deforestation_drivers <- function(shp,
                                        fritz_et_al,
                                        rundir = tempdir(),
                                        verbose = TRUE,
                                        todisk = FALSE,
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
  cropped <- mask(fritz_et_al, shp)
  names(cropped) <- "code"
  arearaster <- cellSize(cropped, unit = "ha")
  arearaster <- mask(arearaster, shp)
  zonal_stat <- zonal(arearaster, cropped, fun = "sum")

  dplyr::left_join(classes, zonal_stat, by = "code") %>%
    tidyr::replace_na(list(area = 0)) %>%
    dplyr::select(class, area) %>%
    dplyr::mutate(percent = area / sum(area)) %>%
    tidyr::replace_na(list(percent = 0)) %>%
    tibble::as_tibble()
}
