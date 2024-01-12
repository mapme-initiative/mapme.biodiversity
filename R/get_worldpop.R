#' Population Count layer for year 2000-2020
#'
#' This resource is published by open spatial demographic data and research
#' organization called WorldPop. This resource represents the population
#' count, 1 km spatial resolution layers available to download from the year
#' 2000 to 2020. The dataset is called as WorldPop Unconstrained Global Mosaics.
#' The encoded cell value represents the total number of people in that particular
#' grid cell.
#'
#' @name worldpop
#' @docType data
#' @keywords resource
#' @format Global raster layers available for years 2000-2020.
#' @source \url{https://www.worldpop.org/}
NULL


#' Downloads Population Count layer
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @keywords internal
#' @include register.R
#' @noRd
.get_worldpop <- function(x,
                          rundir = tempdir(),
                          verbose = TRUE) {
  target_years <- attributes(x)$years
  available_years <- 2000:2020
  target_years <- check_available_years(
    target_years, available_years, "popcount"
  )

  urls <- unlist(sapply(target_years, function(year) .get_worldpop_url(year)))
  fps <- purrr::map_dfr(urls, function(url){
    st_bbox(c(xmin=-180.00125, ymin=-71.99208, xmax=179.99875, ymax=83.99958), crs = "EPSG:4326") %>%
      st_as_sfc() %>%
      st_as_sf() %>%
      dplyr::mutate(source = url)
  })
  fps <- make_footprints(
    fps, what = "vector",
    opts = c("-a_ullr", "-180.00125 -71.99208 179.99875 83.99958",
             "-co", "COMPRESS=LZW"))
  fps
}


#' Helper function to construct population layer urls
#'
#' @param target_year A numeric indicating the target year
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
.get_worldpop_url <- function(target_year) {
  available_years <- c(2000:2020)
  if (target_year %in% available_years) {
    paste0(
      "https://data.worldpop.org/GIS/Population/Global_2000_2020/",
      target_year, "/0_Mosaicked/ppp_", target_year, "_1km_Aggregated.tif"
    )
  } else {
    warning(
      sprintf(
        "Population count not available for target year %s", target_year
      )
    )
    NULL
  }
}


register_resource(
  name = "worldpop",
  type = "raster",
  source = "https://www.worldpop.org/",
  fun = .get_worldpop,
  arguments <- list()
)
