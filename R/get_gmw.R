#' Global Mangrove Extent Polygon
#'
#' This resource is part of the publication by Bunting et al. (2018)
#' "The Global Mangrove Watch—A New 2010 Global Baseline of Mangrove Extent".
#' The polygons represent the mangrove, which is tropical coastal vegetation
#' and considered the most significant part of the marine ecosystem. This
#' resource is available for the period 1996- 2020 from Global Mangrove Watch
#' (GMW), providing geospatial information about global mangrove extent.
#'
#'
#' @name gmw
#' @docType data
#' @keywords resource
#' @format Global mangrove extent polygon available for years 1996, 2007-2010,
#'   and 2015-2020.
#' @references Bunting P., Rosenqvist A., Lucas R., Rebelo L-M., Hilarides L.,
#' Thomas N., Hardy A., Itoh T., Shimada M. and Finlayson C.M. (2018). The Global
#' Mangrove Watch – a New 2010 Global Baseline of Mangrove Extent. Remote Sensing
#' 10(10): 1669. doi:10.3390/rs10101669.
#' @source \url{https://data.unep-wcmc.org/datasets/45}
NULL


#' Downloads Global Mangrove Extent Polygon
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @keywords internal
#' @include register.R
#' @noRd
.get_gmw <- function(x,
                     rundir = tempdir(),
                     verbose = TRUE) {
  target_years <- attributes(x)$years
  available_years <- c(1996, 2007:2010, 2015:2017, 2019:2020)
  target_years <- check_available_years(
    target_years, available_years, "mangroveextent"
  )
  # 2018 is in pseudo-mercator
  urls <- unlist(sapply(target_years, function(year) .get_mangrove_url(year)))
  fps <- make_footprints(urls, "vector")
  fps[["filename"]] <- purrr::map_chr(urls, function(x) strsplit(x, "/")[[1]][8])
  fps
}


#' A helper function to construct correct mangrove layer urls
#'
#' @param target_year A numeric indicating the target year
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_mangrove_url <- function(target_year) {
  base_url <-  paste0("/vsizip//vsicurl/https://datadownload-production.s3.amazonaws.com/GMW_v3_", target_year, ".zip")
  path <- switch(as.character(target_year),
         "2018" = paste0("GMW_v3_2018/00_Data/gmw_v3_2018.shp"),
         paste0("gmw_v3_", target_year, "_vec.shp"))
  file.path(base_url, path)
}


register_resource(
  name = "gmw",
  type = "vector",
  source = "https://data.unep-wcmc.org/datasets/45",
  fun = .get_gmw,
  arguments <- list()
)
