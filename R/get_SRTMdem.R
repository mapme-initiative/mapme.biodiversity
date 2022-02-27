.get_SRTMdem <- function(x,
                         rundir = tempdir(),
                         verbose = TRUE) {

  bbox = st_bbox(x)

  # make the SRTM grid and construct urls for intersecting tiles
  grid_srtm = .makeSRTMGrid()
  tile_ids = st_intersects(st_as_sfc(bbox), grid_srtm)[[1]]
  if(length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the SRTM grid.")
  urls = unlist(sapply(tile_ids, function(tile) .getSrtmURL(tile)))

  # start download in a temporal directory within tmpdir
  if(verbose) pb = progress_bar$new(total = length(urls))
  for (url in urls){

    tryCatch(
      {
        if(verbose) pb$tick(0)
        download.file(url, file.path(rundir, basename(url)), quiet = TRUE)
        if(verbose) pb$tick()

      }, error = function(e) {
        message('reading URLs!')
      }
    )
  }

  # get path of all the zip files
  all_zips = list.files(rundir, full.names = T)
  sapply(all_zips, function(zip) .unzipSRTM(zip, rundir))
  # remove all except desired layers
  d_files = list.files(rundir, full.names = T)
  unlink(grep("*tif", d_files, value = T, invert = T), recursive = T, force = T)
  # return paths to the rasters
  list.files(rundir, full.names = T)

}





.makeSRTMGrid <- function(xmin=-180, xmax=180, dx=5, ymin=-60, ymax=60, dy=5,
                         proj=NULL) {
  if (is.null(proj)) proj = st_crs(4326)
  ncells = c((xmax - xmin) / dx,
             (ymax - ymin) / dy)

  bbox = st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin))
  st_as_sf(st_make_grid(bbox, cellsize = 5, n = ncells, crs = "EPSG:4326", what = "polygons"))

}



.getSrtmURL <- function(tile) {

  index.c <- tile%%72
  index.r <- 24 - floor(tile/72)
  if (tile%%72 == 0) {
    index.c = 72
    index.r = index.r + 1
  }

  tileId <- sprintf("%02d_%02d", index.c, index.r)
  url = paste0("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_",tileId,".zip")
  url
}




.unzipSRTM <- function(zip_files, rundir) {

  unzip(zipfile = file.path(rundir, basename(zip_files)),
        exdir = rundir)
  unlink(paste0(rundir, "/", basename(zip_files)))
}

