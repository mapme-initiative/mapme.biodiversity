#' Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)
#'
#' This resource is published by Funk et al. (2015) and represents a quasi-global
#' (50°S-50°S) rainfall estimation at a monthly resolution starting with the year
#' 1981 to the near-present. It has a spatial resolution of 0.05°. The data can
#' be used to retrieve information on the amount of rainfall. Due to the availability
#' of +30 years, anomaly detection and long-term average analysis is also possible.
#' The routine will download the complete archive in order to support long-term
#' average and anomaly calculations with respect to the 1981 - 2010 climate normal
#' period. Thus no additional arguments need to be specified.
#'
#'
#' @name chirps
#' @param years A numeric vector of the years to download CHIRPS precipitation
#'   layers. Must be greater 1981, defaults to `c(1981:2020)`.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @source \url{https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs/}
#' @references Funk, C., Peterson, P., Landsfeld, M. et al. The climate hazards
#' infrared precipitation with stations—a new environmental record for
#' monitoring extremes. Sci Data 2, 150066 (2015).
#' \doi{10.1038/sdata.2015.66}
#' @include register.R
#' @export
get_chirps <- function(years = 1981:2020) {
  avail_years <- seq(1981, format(Sys.Date(), "%Y"))
  years <- check_available_years(years, avail_years, "chirps")

  function(x,
           name = "chirps",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    urls <- .get_chirps_urls(years)
    filenames <- gsub("cog", "tif", basename(urls))
    co <- c("-of", "COG", "-co", "COMPRESSION=LZW", "-a_nodata", "-9999")

    bbox <- c(xmin = -180., ymin = -50., xmax = 180., ymax = 50.)
    fps <- st_as_sfc(st_bbox(bbox, crs = "EPSG:4326"))
    fps <- st_as_sf(rep(fps, length(urls)))
    fps[["source"]] <- urls

    make_footprints(fps, what = "raster", filenames = filenames)
  }
}

.get_chirps_urls <- function(years = 1981:2020) {
  chirps_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs/"
  rsp <- httr2::req_perform(httr2::request(chirps_url))
  httr2::resp_check_status(rsp)
  chirps_list <- as.character(httr2::resp_body_html(rsp))
  chirps_list <- regmatches(chirps_list, gregexpr(chirps_list, pattern = "<a href=\"(.*?)\""))
  chirps_list <- gsub(".*\"([^`]+)\".*", "\\1", chirps_list[[1]])
  chirps_list <- grep("*.cog$", chirps_list, value = TRUE)

  chirps_list <- grep(
    pattern = paste(years, collapse = "|"),
    chirps_list, value = TRUE
  )

  paste("/vsicurl/", chirps_url, chirps_list, sep = "")
}

register_resource(
  name = "chirps",
  description = "Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)",
  licence = "CC - unknown",
  source = "https://www.chc.ucsb.edu/data/chirps",
  type = "raster"
)
