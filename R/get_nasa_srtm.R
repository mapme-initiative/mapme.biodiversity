#' SRTM 30m Digital Elevation Model (DEM) layer
#'
#' This resource is published by Jarvis et al. (2008) "Hole-filled seamless SRTM data V4".
#' The layer represents the 30m global terrestrial digital elevation model
#' from the NASA Shuttle Radar Topographic Mission (SRTM), available for download as
#' 5 degree x 5 degree tiles. It is encoded as meter, representing the elevation
#' at the particular grid cell.
#'
#' **Important Note**:
#'
#' As of January 2023, the SSL certificate of the website where this resource is downloaded
#' from has expired. We include a check and inform users that this resource might not
#' be downloaded without unsetting SSL certification checks on their own risk.
#' The maintainers of the website have been contacted and asked to renew the
#' certificate. Until then, users can decide to disable the SSL certification
#' checks for their preferred downloading method. Using wget as an example, the
#' following options can be set to disable the SSL checks at your own risk:
#'
#' \code{httr::set_config(httr::config(ssl_verifypeer = 0L))}
#' \code{options(download.file.method="wget", download.file.extra="--no-check-certificate")}
#'
#' This assumes that your are *not* using the aria2 downloader.
#'
#' @name nasa_srtm
#' @docType data
#' @keywords resource
#' @format A global tiled raster resource available for all land areas.
#' @references Jarvis A., H.I. Reuter, A. Nelson, E. Guevara, 2008,
#' Hole-filled seamless SRTM data V4, International Centre for Tropical Agriculture (CIAT).
#' @source https://srtm.csi.cgiar.org/
NULL


#' Downloads SRTM 30m Digital Elevation Model (DEM) Layer
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @keywords internal
#' @noRd

.get_nasa_srtm <- function(x,
                           rundir = tempdir(),
                           verbose = TRUE) {
  .warn_about_ssl()
  # make the SRTM grid and construct urls for intersecting tiles
  grid_srtm <- .make_global_grid(
    xmin = -180, xmax = 180, dx = 5,
    ymin = -60, ymax = 60, dy = 5
  )
  tile_ids <- unique(unlist(st_intersects(x, grid_srtm)))
  if (length(tile_ids) == 0) {
    stop("The extent of the portfolio does not intersect with the SRTM grid.")
  }
  urls <- unlist(sapply(tile_ids, function(tile) .get_srtm_url(tile)))

  if (attr(x, "testing")) {
    return(basename(urls))
  }

  filenames <- file.path(rundir, basename(urls))
  # start download in a temporal directory within tmpdir
  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(
    urls = urls,
    filenames = filenames,
    verbose = verbose,
    aria_bin = aria_bin,
    check_existence = TRUE
  )
  # unzip zip files
  sapply(filenames, function(zip) .unzip_and_remove(zip, rundir, remove = FALSE))
  # return paths to the rasters
  gsub(".zip$", ".tif", filenames)
}


#' Helper function to construct SRTM urls
#'
#' @param tile An sf object representing the tile to download
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_srtm_url <- function(tile) {
  index.c <- tile %% 72
  index.r <- 24 - floor(tile / 72)
  if (tile %% 72 == 0) {
    index.c <- 72
    index.r <- index.r + 1
  }

  tileId <- sprintf("%02d_%02d", index.c, index.r)
  paste0(
    "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_",
    tileId, ".zip"
  )
}

.warn_about_ssl <- function(){
  status <- try(httr::http_error("https://srtm.csi.cgiar.org/"), silent = TRUE)
  if(inherits(status, "try-error")){
    msg <- paste0(
      "The data source for 'nasa_srtm' has an expired SSL certificate.\n ",
      "You can disable SSL checks if you still want to download the resource.\n ",
      "Note, that when disabling SSL certification you understand and accept\n ",
      "the associated risks. See `?nasa_srtm` for further information."
    )
    stop(msg)
  }
}
