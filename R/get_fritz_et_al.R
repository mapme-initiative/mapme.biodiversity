#' Drivers of deforestation for tropical forests
#'
#' This resource is produced by a neirest-neighbour matching of a crowd-sourced
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
#' The following argument should be specified:
#'
#' \describe{
#'   \item{res_drivers}{An integer indicating the resolution
#'   to download. Defaults to 100.}
#'   }
#'
#' @name fritz_et_al
#' @docType data
#' @keywords resource
#' @format Global raster layer available of deforestation drivers for the
#'   period 2008-2019.
#' @references Steffen, F., Carlos, J.C.L., See. L., Schepaschenko D.,
#' Hofhansl F., Jung M., Dürauer M., Georgieva I., Danylo O., Lesiv M.,
#' McCallum I. (2022) A Continental Assessment of the Drivers of Tropical
#' Deforestation With a Focus on Protected Areas. F.Cos.Sc.(3)
#' doi:10.3389/fcosc.2022.830248
#' @source \url{https://zenodo.org/record/7997885}
NULL


#' @include register.R
.get_fritz_et_al <- function(x,
                             res_drivers = 100,
                             rundir = tempdir(),
                             verbose = TRUE) {
  urls <- c(
    "/vsizip//vsicurl/https://zenodo.org/record/7997885/files/Deforestation%20Drivers%20100m%20IIASA.zip/dr_han_hei_pr1.tif",
    "/vsizip//vsicurl/https://zenodo.org/record/7997945/files/Deforestation%20drivers%201km%20IIASA%20.zip/dr_heine_pr1.tif"
  )

  if (!res_drivers %in% c(100, 1000)) {
    stop("Fritz et al. resource is available only at resolutions 100 and 1.000.")
  }

  url <- ifelse(res_drivers == 100, urls[1], urls[2])
  make_footprints(url, paste0("geo_fritz_et_al_", res_drivers, "m.tif"), "raster")
}

register_resource(
  name = "fritz_et_al",
  type = "raster",
  source = "https://zenodo.org/record/7997885/",
  fun = .get_fritz_et_al,
  arguments <- list(res_drivers = 100)
)
