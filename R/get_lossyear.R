.get_lossyear <- function(bbox,
                           vers = "GFC-2018-v1.6",
                           tmpdir = tempfile()){
  # make the GFW grid and construct urls for intersecting tiles
  grid_GFC = .makeGFWGrid()
  tile_ids = st_intersects(st_as_sfc(bbox), grid_GFC)[[1]]
  if(length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the GFW grid.")
  urls = sapply(tile_ids, function(n) .getGFWTileURL(grid_GFC[n,], vers, "lossyear"))

  # start download in a temporal directory within tmpdir
  rundir = tempfile(tmpdir = tmpdir)
  dir.create(rundir)
  for (url in urls){
    download.file(url, file.path(rundir, basename(url)))
    #command = sprintf("gdal_translate %s %s -of COG -co COMPRESS=LZW", url, file.path(rundir, basename(url)))
    #system(command)
  }
  # return all paths to the downloaded files
  list.files(rundir, full.names = T)
}
