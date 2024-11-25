#' Climatologies at High resolution for the Earth Land Surface Areas (CHELSA)
#'
#' The CHELSA data (Karger et al. 2017) consists of downscaled model output
#' temperature and precipitation estimates at a horizontal resolution of 30 arc
#' sec. The precipitation algorithm incorporates orographic predictors including
#' wind fields, valley exposition, and boundary layer height, with a subsequent
#' bias correction. The spatial resolution is about 1-arc second (~1km at the
#' equator). This resource makes V2 available.
#'
#'
#' @name chelsa
#' @param years A numeric vector of the years to make CHELSA monthly precipitation
#'   layers available for. Must be greater 1979, defaults to `c(1979:2019)`.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @source \url{https://envicloud.wsl.ch/#/?prefix=chelsa/chelsa_V2/GLOBAL/}
#' @references Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H.,
#'   Soria-Auza, R.W., Zimmermann, N.E., Linder, H.P. & Kessler, M. (2021)
#'   Climatologies at high resolution for the earth’s land surface areas.
#'   EnviDat. \doi{https://doi.org/10.16904/envidat.228.v2.1}
#'
#'   Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza,
#'   R.W., Zimmermann, N.E., Linder, P., Kessler, M. (2017): Climatologies at high
#'   resolution for the Earth land surface areas. Scientific Data. 4
#'   170122. \doi{https://doi.org/10.1038/sdata.2017.122}
#' @include register.R
#' @export
get_chelsa <- function(years = 1979:2019) {
  years <- check_available_years(years, 1979:2019, "chelsa")

  function(x,
           name = "chelsa",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    urls <- .get_chelsa_urls(years)
    co <- c("-ot", "Int32", "-co", "COMPRESS=DEFLATE", "-co", "PREDICTOR=2", "-a_nodata", "-2147483647")
    bbox <- c(xmin = -180., ymin = -90., xmax = 180., ymax = 84.)
    fps <- st_as_sfc(st_bbox(bbox, crs = "EPSG:4326"))
    fps <- st_as_sf(rep(fps, length(urls)))
    fps[["source"]] <- urls

    make_footprints(fps, what = "raster", co = co)
  }
}

.get_chelsa_urls <- function(years = 1979:2019) {
  url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/pr/"
  files <- paste(sprintf("%02d", 1:12), rep(years, each = 12), sep = "_")
  files <- sprintf("CHELSA_pr_%s_V.2.1.tif", files)
  paste("/vsicurl/", url, files, sep = "")
}

register_resource(
  name = "chelsa",
  description = "Climatologies at High resolution for the Earth Land Surface Areas (CHELSA)",
  licence = "Unknown - Must cite!",
  source = "https://chelsa-climate.org/",
  type = "raster"
)
