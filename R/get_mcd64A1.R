#' MODIS Burned Area Monthly (MCD64A1)
#'
#' The Terra and Aqua combined MCD64A1 Version 6.1 Burned Area data product is
#' a monthly, global gridded 500 meter (m) product containing per-pixel
#' burned-area and quality information. The MCD64A1 burned-area mapping approach
#' employs 500 m Moderate Resolution Imaging Spectroradiometer (MODIS) Surface
#' Reflectance imagery coupled with 1 kilometer (km) MODIS active fire observations.
#'
#' The algorithm uses a burn sensitive Vegetation Index (VI) to create dynamic
#' thresholds that are applied to the composite data. The VI is derived from
#' MODIS shortwave infrared atmospherically corrected surface reflectance bands
#' 5 and 7 with a measure of temporal texture. The algorithm identifies the date
#' of burn for the 500 m grid cells within each individual MODIS tile. The date
#' is encoded in a single data layer as the ordinal day of the calendar year on
#' which the burn occurred with values assigned to unburned land pixels and
#' additional special values reserved for missing data and water grid cells.
#'
#' @name mcd64a1
#' @param years Numeric vector of years to make the MCD64A1 product available
#'   for. Must be greater than the year 2000.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Giglio, L., C. Justice, L. Boschetti, D. Roy. MODIS/Terra+Aqua
#'   Burned Area Monthly L3 Global 500m SIN Grid V061. 2021, distributed by
#'   NASA EOSDIS Land Processes Distributed Active Archive Center.
#'   \doi{https://doi.org/10.5067/MODIS/MCD64A1.061}
#' @source \url{https://planetarycomputer.microsoft.com/dataset/modis-64A1-061}
#' @include register.R
#' @export
get_mcd64a1 <- function(years = 2000:2022) {
  check_namespace("rstac")
  now <- as.numeric(format(Sys.Date(), "%Y"))
  years <- check_available_years(years, c(2000:now), "mcd64a1")

  function(x,
           name = "mcd64a1",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    dt <- paste0(min(years), "-01-01/", max(years), "-12-31")
    items <- try(rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
      rstac::stac_search(
        collection = "modis-64A1-061",
        bbox = as.numeric(st_bbox(x)),
        datetime = dt,
        limit = NULL
      ) %>%
      rstac::post_request() %>%
      rstac::items_fetch())

    if (inherits(items, "try-error")) {
      stop("Download for MCD64A1 resource was unsuccesfull")
    }

    urls <- rstac::assets_url(items, asset_names = "Burn_Date")
    if (length(urls) == 0) {
      stop("The extent of the portfolio does not intersect with the SRTM grid.")
    }

    bboxs <- rstac::items_bbox(items)
    fps <- purrr::map(bboxs, function(x) {
      names(x) <- c("xmin", "ymin", "xmax", "ymax")
      bbox <- st_bbox(x, crs = "EPSG:4326")
      st_as_sf(st_as_sfc(bbox))
    })
    fps <- st_as_sf(purrr::list_rbind(fps))
    fps[["source"]] <- paste0("/vsicurl?pc_url_signing=yes&url=", urls)
    fps <- st_transform(fps, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
    make_footprints(
      fps,
      what = "raster",
      co = c("-co", "COMPRESS=DEFLATE", "-of", "COG"),
      precision = 1e3
    )
  }
}

register_resource(
  name = "mcd64a1",
  description = "MODIS Burned Area Monthly Product (Aqua and Terra)",
  licence = "https://lpdaac.usgs.gov/data/data-citation-and-policies/",
  source = "https://planetarycomputer.microsoft.com/dataset/modis-64A1-061",
  type = "raster"
)
