#' Global Mangrove Extent Polygon
#'
#' This resource is part of the publication by Bunting et al. (2018)
#' "The Global Mangrove Watch—A New 2010 Global Baseline of Mangrove Extent".
#' The polygons represent the mangrove, which is tropical coastal vegetation
#' and considered the most significant part of the marine ecosystem. This
#' resource is available for the selected years in the period 1996- 2020 from
#' Global Mangrove Watch (GMW), providing geospatial information about global
#' mangrove extent.
#'
#'
#' @name gmw
#' @param years A numeric vector of the years for which to make GMW available.
#' @docType data
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Bunting P., Rosenqvist A., Lucas R., Rebelo L-M., Hilarides L.,
#' Thomas N., Hardy A., Itoh T., Shimada M. and Finlayson C.M. (2018). The Global
#' Mangrove Watch – a New 2010 Global Baseline of Mangrove Extent. Remote Sensing
#' 10(10): 1669. doi:10.3390/rs10101669.
#' @source \url{https://data.unep-wcmc.org/datasets/45}
#' @include register.R
#' @export
get_gmw <- function(years = c(1996, 2007:2010, 2015:2020)) {
  avail_years <- c(1996, 2007:2010, 2015:2020)
  years <- check_available_years(years, avail_years, "gmw")

  function(x,
           name = "gmw",
           type = "vector",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    urls <- unlist(sapply(years, function(year) .get_mangrove_url(year)))
    bbox <- c(xmin = -180., ymin = -38.85822, xmax = 180., ymax = 32.36822)
    fps <- st_as_sfc(st_bbox(bbox, crs = "EPSG:4326"))
    fps <- st_as_sf(rep(fps, length(urls)))
    fps[["source"]] <- urls
    fps <- make_footprints(
      fps,
      filenames = gsub(".shp", ".gpkg", basename(urls)),
      what = "vector"
    )

    if (2018 %in% years) {
      i <- which(years == 2018)
      fps[["co"]][i] <- list(c("-t_srs", "EPSG:4326"))
      fps[["filename"]][i] <- "gmw_v3_2018_vec.gpkg"
    }

    fps
  }
}

#' A helper function to construct correct mangrove layer urls
#'
#' @param target_year A numeric indicating the target year
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_mangrove_url <- function(target_year) {
  base_url <- paste0("/vsizip//vsicurl/https://datadownload-production.s3.amazonaws.com/GMW_v3_", target_year, ".zip")
  path <- switch(as.character(target_year),
    "2018" = paste0("GMW_v3_2018/00_Data/gmw_v3_2018.shp"),
    paste0("gmw_v3_", target_year, "_vec.shp")
  )
  file.path(base_url, path)
}


register_resource(
  name = "gmw",
  description = "Global Mangrove Watch - Vector data of mangrove extent",
  licence = "CC BY 4.0",
  source = "https://data.unep-wcmc.org/datasets/45",
  type = "vector"
)
