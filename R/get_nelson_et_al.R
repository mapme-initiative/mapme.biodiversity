#' Accessibility to Cities layer
#'
#' This resource is published by Weiss et al. (2018) "A global map of travel
#' time to cities to assess inequalities in accessibility in 2015" on journal
#' nature. Accessibility is the ease with which larger cities can be reached
#' from a certain location. This resource represents the travel time to major
#' cities in the year 2015. Encoded as minutes, representing the time needed
#' to reach that particular cell from nearby city of target population range.
#' The following ranges to nearby cities are available:
#' - "5k_10k"
#' - "10k_20k"
#' - "20k_50k"
#' - "50k_100k"
#' - "100k_200k"
#' - "200k_500k"
#' - "500k_1mio"
#' - "1mio_5mio"
#' - "50k_50mio"
#' - "5k_110mio"
#' - "20k_110mio"
#' - "5mio_50mio"
#'
#' @note Note, that the 'figshare' server applies a rather restrictive rate limit
#'   thus frequently resulting in opaque error codes (see \url{https://github.com/mapme-initiative/mapme.biodiversity/issues/308}).
#'   Please set GDAL configuration options to sensible values in case
#'   you are running into this issue, e.g.: \code{Sys.setenv("GDAL_HTTP_MAX_RETRY" = "5", "GDAL_HTTP_RETRY_DELAY" = "15")}.
#'
#' @name nelson_et_al
#' @param ranges A character vector indicating one or more ranges
#'   to download.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Weiss, D. J., Nelson, A., Gibson, H. S., Temperley, W., Peedell,
#' S., Lieber, A., … & Gething, P. W. (2018). A global map of travel time to cities
#' to assess inequalities in accessibility in 2015. Nature, 553(7688), 333-336.
#' @source \url{https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3}
#' @include register.R
#' @export
get_nelson_et_al <- function(ranges = "20k_50k") {
  if (any(!ranges %in% .nelson_df$range)) {
    index <- which(!ranges %in% .nelson_df$range)
    ranges <- ranges[-index]
    if (length(ranges) == 0) {
      stop("No supoorted ranges have been specified.")
    }
  }

  function(x,
           name = "nelson_et_al",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    # get url for accessibility layer
    fps <- .get_traveltime_url(ranges, paste0("traveltime-", ranges, ".tif"),
      verbose = verbose
    )

    make_footprints(fps,
      filenames = fps[["filename"]], what = "raster",
      co = c("-co", "COMPRESS=LZW", "-ot", "UInt16", "-a_nodata", "65535")
    )
  }
}


.nelson_df <- data.frame(
  range = c(
    "5k_10k", "10k_20k", "20k_50k", "50k_100k", "100k_200k", "200k_500k",
    "500k_1mio", "1mio_5mio", "50k_50mio", "5k_110mio", "20k_110mio", "5mio_50mio"
  ),
  index = c(
    14189840, 14189837, 14189831, 14189825, 14189819, 14189816, 14189810,
    14189807, 14189849, 14189852, 14189843, 14189804
  )
)

.get_traveltime_url <- function(range, filenames, verbose = TRUE) {
  urls <- unlist(lapply(range, function(x) {
    index <- .nelson_df$index[.nelson_df$range == x]
    paste0("/vsicurl/https://figshare.com/ndownloader/files/", index)
  }))

  bbox <- c(xmin = -180., ymin = -60., xmax = 180., ymax = 85.)
  fps <- st_as_sfc(st_bbox(bbox, crs = "EPSG:4326"))
  fps <- st_as_sf(rep(fps, length(urls)))
  fps[["source"]] <- urls
  fps[["filename"]] <- filenames
  fps
}

register_resource(
  name = "nelson_et_al",
  description = "Global maps of traveltime to cities",
  licence = "CC-BY 4.0",
  source = "https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3",
  type = "raster"
)
