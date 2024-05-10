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
#' @param years A numeric vector indicating the years for which to make the
#'   resource available.
#' @docType data
#' @keywords resource
#' @returns A function that returns a character of file paths.
#' @source \url{https://www.worldpop.org/}
#' @include register.R
#' @export
get_worldpop <- function(years = 2000) {
  years <- check_available_years(years, c(2000:2020), "worldpop")

  function(x,
           name = "worldpop",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    urls <- unlist(sapply(years, function(year) .get_worldpop_url(year)))
    bbox <- c(xmin = -180.00125, ymin = -71.99208, xmax = 179.99875, ymax = 83.99958)
    tiles <- st_as_sfc(st_bbox(bbox, crs = "EPSG:4326"))
    tiles <- st_as_sf(rep(tiles, length(urls)))
    tiles[["source"]] <- urls
    make_footprints(tiles,
      what = "raster",
      co = c(
        "-a_ullr", "-180.00125 -71.99208 179.99875 83.99958",
        "-co", "COMPRESS=LZW"
      )
    )
  }
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
      "/vsicurl/https://data.worldpop.org/GIS/Population/Global_2000_2020/",
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
  description = "WorldPop - Unconstrained Global Mosaics 2000 - 2020",
  licence = "CC-BY 4.0",
  source = "https://www.worldpop.org/",
  type = "raster"
)
