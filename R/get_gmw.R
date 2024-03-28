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
#' @param years A numeric vector of the years for which to make GMW available.
#' @docType data
#' @keywords resource
#' @returns A function that returns a character of file paths.
#' @references Bunting P., Rosenqvist A., Lucas R., Rebelo L-M., Hilarides L.,
#' Thomas N., Hardy A., Itoh T., Shimada M. and Finlayson C.M. (2018). The Global
#' Mangrove Watch – a New 2010 Global Baseline of Mangrove Extent. Remote Sensing
#' 10(10): 1669. doi:10.3390/rs10101669.
#' @source \url{https://data.unep-wcmc.org/datasets/45}
#' @include register.R
#' @export
get_gmw <- function(years = c(1996, 2007:2010, 2015:2020)) {
  avail_years <- c(1996, 2007:2010, 2015:2020)
  years <- check_available_years(years, avail_years, "gmw")

  function(x,
           name = "gmw",
           type = "vector",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    urls <- unlist(sapply(years, function(year) .get_mangrove_url(year)))
    filenames <- file.path(outdir, basename(paste0("gmw-extent_", years, ".zip")))
    if (testing) {
      return(basename(filenames))
    }
    filenames <- download_or_skip(urls, filenames, check_existence = FALSE)
    if (verbose) {
      message("Translating shapefiles to GeoPackages. This may take a while....")
    }
    sapply(filenames, function(zip) .unzip_mangrove(zip, outdir))
    out <- list.files(outdir, full.names = T, pattern = ".gpkg")
    grep(paste(years, collapse = "|"), out, value = TRUE)
  }
}


#' Helper function to correctly unzip a mangrove layer zip file
#'
#'
#' @param zip_files A character vector with potentially multiple zip files
#' @param dir The directory to where the files are unzipped
#'
#' @return Nothing.
#' @keywords internal
#' @noRd
.unzip_mangrove <- function(zip, dir) {
  bn <- basename(zip)
  year <- gsub(".*?([0-9]+).*", "\\1", bn)
  gpkg <- file.path(dir, paste0("gmw-extent_", year, ".gpkg"))
  if (file.exists(gpkg)) {
    return(gpkg)
  }
  utils::unzip(
    zipfile = file.path(
      dir,
      basename(paste0("gmw-extent_", year, ".zip"))
    ),
    exdir = dir
  )

  # Source data from 2018 doesn't correspond to the pattern for other years.
  # We need to create an exception for it.
  if (year == 2018) {
    shp <- file.path(
      dir,
      paste0("GMW_v3_2018/00_Data/gmw_v3_", year, ".shp")
    )
  } else {
    shp <- file.path(dir, paste0("gmw_v3_", year, "_vec.shp"))
  }

  gdal_utils(
    util = "vectortranslate", shp, gpkg,
    options = c("-t_srs", "EPSG:4326")
  )

  d_files <- list.files(dir, full.names = T)
  unlink(grep("gmw-extent*", d_files, value = T, invert = T),
    recursive = T, force = T
  )
}


#' A helper function to construct correct mangrove layer urls
#'
#' @param target_year A numeric indicating the target year
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_mangrove_url <- function(target_year) {
  available_years <- c(1996, 2007:2010, 2015:2020)
  if (target_year %in% available_years) {
    paste0("https://wcmc.io/GMW_", target_year)
  } else {
    warning(
      sprintf(
        "Mangove extent not available for target year %s", target_year
      )
    )
    NULL
  }
}


register_resource(
  name = "gmw",
  description = "Global Mangrove Watch - Vector data of mangrove extent",
  licence = "CC BY 4.0",
  source = "https://data.unep-wcmc.org/datasets/45",
  type = "vector"
)
