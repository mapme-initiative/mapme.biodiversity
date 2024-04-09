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
#' @name fritz_et_al
#' @param resolution An integer indicating the resolution
#'   to download. Defaults to 100.
#' @keywords resource
#' @returns A function that returns a character of file paths.
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
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    urls <- c(
      "https://zenodo.org/record/7997885/files/Deforestation%20Drivers%20100m%20IIASA.zip?download=1",
      "https://zenodo.org/record/7997945/files/Deforestation%20drivers%201km%20IIASA%20.zip?download=1"
    )

    url <- ifelse(resolution == 100, urls[1], urls[2])
    filename <- sub(".*/(.*\\..*)\\?.*", "\\1", utils::URLdecode(url))
    filename <- file.path(outdir, gsub("\\s+", "_", filename))

    if (testing) {
      return(basename(filename))
    }

    download_or_skip(url, filename, check_existence = FALSE)
    unzip_and_remove(filename, outdir, remove = FALSE)
    files <- list.files(outdir, full.names = TRUE)
    identifier <- paste0("geo_fritz_et_al_", resolution, "m.tif")
    files_outdir <- list.files(outdir, full.names = TRUE)
    geo_file <- grep(identifier, files, value = TRUE)
    if (length(geo_file) > 0) {
      return(geo_file)
    }
    tif_file <- grep("*.tif$", files, value = TRUE)
    if (length(tif_file) > 1) {
      tif_file <- grep("geo", tif_file, value = TRUE, invert = TRUE)
    }
    geo_file <- file.path(outdir, identifier)
    sf::gdal_utils("warp", tif_file, geo_file,
      options = c(
        "-t_srs", "EPSG:4326",
        "-r", "near",
        "-ot", "Byte",
        "-co", "COMPRESS=LZW"
      )
    )

    # delete all files accept with geo component in name
    del_files <- grep("geo_", list.files(outdir, full.names = TRUE),
      value = TRUE, invert = TRUE
    )
    # exclude zip from files to delete
    del_files <- grep("zip", del_files, value = TRUE, invert = TRUE)
    file.remove(del_files)
    return(geo_file)
  }
}

register_resource(
  name = "fritz_et_al",
  licence = "CC-BY 4.0",
  description = "Drivers of deforestation in the tropics",
  source = "https://zenodo.org/record/7997885/",
  type = "raster"
)
