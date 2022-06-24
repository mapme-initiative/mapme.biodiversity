#' SRTM 30m Digital Elevation Model (DEM) layer
#'
#' This resource is published by Farr et al. (2007) "The Shuttle Radar Topography
#' Mission". The layer represents the 30m global terrestrial digital elevation model
#' from the NASA Shuttle Radar Topographic Mission (SRTM), available for download as
#' 5 degree x 5 degree tiles. It is encoded as meter, representing the elevation
#' at the particular grid cell.
#'
#' @name srtmdem
#' @docType data
#' @keywords resource
#' @format A global tiled raster resource available for all land areas.
#' @references NASA Shuttle Radar Topography Mission (SRTM)(2013). Shuttle Radar
#' Topography Mission (SRTM) Global. Distributed by OpenTopography.
#' https://doi.org/10.5069/G9445JDF. Accessed: 2022-03-17
#' @source \url{https://srtm.csi.cgiar.org/}
NULL


#' Downloads SRTM 30m Digital Elevation Model (DEM) Layer
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @keywords internal
#' @noRd

.get_srtmdem <- function(x,
                         rundir = tempdir(),
                         verbose = TRUE) {
  bbox <- st_bbox(x)
  # make the SRTM grid and construct urls for intersecting tiles
  grid_srtm <- .make_global_grid(
    xmin = -180, xmax = 180, dx = 5,
    ymin = -60, ymax = 60, dy = 5
  )
  tile_ids <- st_intersects(st_as_sfc(bbox), grid_srtm)[[1]]
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
