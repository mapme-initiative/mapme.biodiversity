#' Downloads SRTM 30m Digital Elevation Model (DEM) Layer
#'
#' The layer represents the 30m global terrestrial digital elevation model from
#' the NASA Shuttle Radar Topographic Mission (SRTM), available for download as
#' 5 degree x 5 degree tiles. It is encoded as meter, representing the elevation
#' at the particular grid cell.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name SRTM_Digital_Elevation_Model
#' @keywords internal
#'

.get_SRTMdem <- function(x,
                         rundir = tempdir(),
                         verbose = TRUE) {
  bbox = st_bbox(x)
  # make the SRTM grid and construct urls for intersecting tiles
  grid_srtm = .makeGlobalGrid(xmin = -180, xmax = 180, dx = 5, ymin = -60, ymax = 60, dy = 5)
  tile_ids = st_intersects(st_as_sfc(bbox), grid_srtm)[[1]]
  if (length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the SRTM grid.")
  urls = unlist(sapply(tile_ids, function(tile) .getSrtmURL(tile)))
  filenames = file.path(rundir, basename(urls))
  if(any(file.exists(filenames))) message("Skipping existing files in output directory.")
  # start download in a temporal directory within tmpdir
  .downloadOrSkip(urls, filenames, verbose)
  # get path of all the zip files
  all_zips <- list.files(rundir, full.names = T)
  sapply(all_zips, function(zip) .UnzipAndRemove(zip, rundir))
  # return paths to the rasters
  list.files(rundir, full.names = T, pattern = ".tif")
}


#' Helper function to construct SRTM urls
#'
#' @param tile An sf object representing the tile to download
#'
#' @return A charchter vector
#' @keywords internal
.getSrtmURL <- function(tile) {
  index.c <- tile %% 72
  index.r <- 24 - floor(tile / 72)
  if (tile %% 72 == 0) {
    index.c <- 72
    index.r <- index.r + 1
  }

  tileId <- sprintf("%02d_%02d", index.c, index.r)
  paste0("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_", tileId, ".zip")
}

