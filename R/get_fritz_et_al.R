#' Drivers of deforestation for tropical forests
#'
#' This resource is produced by a nearest-neighbour matching of a crowd-sourced
#' campaign to map dominant driver of forest loss based on visual interpretation
#' of VHR images matched with Global Forest Loss data by Hansen (2013) version
#' 1.7 The forest loss layer was re sampled to a resolution of 100 and 1.000
#' meters. Dominant drivers were determined for the period 2008 to 2009.
#'
#' It indicates 9 different classes:
#' - commercial agriculture
#' - commercial oil palm plantations
#' - managed forests
#' - mining
#' - natural disturbances
#' - pasture
#' - roads
#' - wildfire
#' - other subsistence agriculture
#' - shifting cultivation
#'
#' @name fritz_et_al
#' @param resolution An integer indicating the resolution
#'   to download. Defaults to 100.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Steffen, F., Carlos, J.C.L., See. L., Schepaschenko D.,
#' Hofhansl F., Jung M., Dürauer M., Georgieva I., Danylo O., Lesiv M.,
#' McCallum I. (2022) A Continental Assessment of the Drivers of Tropical
#' Deforestation With a Focus on Protected Areas. F.Cos.Sc.(3)
#' doi:10.3389/fcosc.2022.830248
#' @source \url{https://zenodo.org/record/7997885}
#' @include register.R
#' @export
get_fritz_et_al <- function(resolution = 100) {
  if (!resolution %in% c(100, 1000)) {
    stop("Fritz et al. resource is available only at resolutions 100 and 1.000.")
  }

  function(x,
           name = "fritz_et_al",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    urls <- c(
      "/vsizip//vsicurl/https://zenodo.org/record/7997885/files/Deforestation%20Drivers%20100m%20IIASA.zip/dr_han_hei_pr1.tif",
      "/vsizip//vsicurl/https://zenodo.org/record/7997945/files/Deforestation%20drivers%201km%20IIASA%20.zip/dr_heine_pr1.tif"
    )

    url <- ifelse(resolution == 100, urls[1], urls[2])
    filename <- paste0("geo_fritz_et_al_", resolution, "m.tif")
    co <- c("-of", "COG", "-ot", "Byte", "-co", "COMPRESS=LZW")
    bbox <- c(xmin = -20037507, ymin = -3341359, xmax = 20038000, ymax = 3340559)
    tile <- st_as_sf(st_as_sfc(st_bbox(bbox, crs = "ESRI:54052")))
    tile[["source"]] <- url
    make_footprints(tile, filename, what = "raster", co = co)
  }
}

register_resource(
  name = "fritz_et_al",
  licence = "CC-BY 4.0",
  description = "Drivers of deforestation in the tropics",
  source = "https://zenodo.org/record/7997885/",
  type = "raster"
)
