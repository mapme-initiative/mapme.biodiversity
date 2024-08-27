#' Accessibility to Cities in 2000
#'
#' This resource provides global maps of travel time to cities of 50,000 or more
#' people in year 2000. Accessibility refers to the ease with which larger cities can be reached from
#' a certain location. This dataset represents travel time to major cities globally
#' as of the year 2000, encoded in minutes. The data is essential for historical
#' analyses, such as understanding the impact of accessibility on land use and
#' socio-economic outcomes during this period.
#'
#' @name accessibility_2000
#' @keywords resource
#' @return A function that returns an `sf` footprint object.
#' @references European Commission, Joint Research Centre (JRC), Global Accessibility Maps (GAM), 2000.
#' @source https://forobs.jrc.ec.europa.eu/gam
#' @include register.R
#' @export
get_accessibility_2000 <- function() {
  function(x,
           name = "accessibility_2000",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    url <- paste(
      "/vsizip//vsicurl/",
      "https://forobs.jrc.ec.europa.eu/data/products/gam/access_50k.zip/",
      "acc_50k.tif",
      sep = ""
    )

    bbox <- c(xmin = -180.0, ymin = -90.0, xmax = 180.0, ymax = 90.0)
    tile <- st_as_sf(st_as_sfc(st_bbox(bbox, crs = "EPSG:4326")))
    tile[["source"]] <- url
    make_footprints(tile, filenames = "acc_50k.tif", what = "raster",
                    co = c("-co", "COMPRESS=LZW",
                           "-co", "BLOCKXSIZE=128",
                           "-co", "BLOCKYSIZE=128",
                           "-co", "TILED=YES",
                           "-ot", "Int32",
                           "-a_nodata", "-2147483647"))
  }
}

# Registering the resource as done for nelson_et_al
register_resource(
  name = "accessibility_2000",
  description = "Accessibility data for the year 2000 from the Global Accessibility Map project",
  licence = "See JRC data policy: https://joint-research-centre.ec.europa.eu/jrc-mission-statement-work-programme/data-policy_en",
  source = "https://forobs.jrc.ec.europa.eu/gam",
  type = "raster"
)

