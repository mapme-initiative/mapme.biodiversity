#' NASA GRACE-based Drought Indicator layer
#'
#' The resource is published by NASA GRACE Tellus. This data set
#' reflects on potential drought conditions in the shallow groundwater section
#' relative to a reference period spanning from 1948 to 2012.
#' It is available as a global raster with a weekly temporal resolution starting
#' with the year 2003. The value indicates the wetness percentile of a given
#' pixel with regard to the reference period.
#'
#' @name nasa_grace
#' @param years A numeric vector indicating the years for which to make the
#'   resource available.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @include register.R
#' @export
get_nasa_grace <- function(years = 2003:2022) {
  years <- check_available_years(years, c(2003:2022), "nasa_grace")

  function(x,
           name = "nasa_grace",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    urls <- unlist(sapply(years, function(year) .get_nasagrace_url(year)))
    bbox <- c(xmin = -180.0, ymin = -60, xmax = 180, ymax = 90)
    tiles <- st_as_sfc(st_bbox(bbox, crs = "EPSG:4326"))
    tiles <- st_as_sf(rep(tiles, length(urls)))
    tiles[["source"]] <- urls
    make_footprints(tiles, what = "raster", co = c("-co", "COMPRESS=LZW"))
  }
}


#' Helper function to construct GRACE URLs
#'
#' @param target_year
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_nasagrace_url <- function(target_year) {
  available_years <- c(2003:2022)
  dates <- seq.Date(as.Date("2003/02/03"), as.Date("2022/05/16"), by = "week")
  available_dates <- as.integer(format(dates, "%Y%m%d"))
  if (target_year %in% available_years) {
    target_dates <- subset(
      available_dates, substr(available_dates, 1, 4) == target_year
    )
    paste0(
      "/vsicurl/https://nasagrace.unl.edu/globaldata/", target_dates,
      "/gws_perc_025deg_GL_", target_dates, ".tif"
    )
  } else {
    warning(sprintf(
      "Drought indicator not available for target year %s",
      target_year
    ))
    NULL
  }
}

register_resource(
  name = "nasa_grace",
  description = "NASA Gravity Recovery And Climate Experiment (GRACE) - Measurments of Earth's mass and water changes",
  licence = "https://nasagrace.unl.edu/About.aspx",
  source = "https://nasagrace.unl.edu/globaldata/",
  type = "raster"
)
