.get_treecover <- function(bbox,
                           vers = "GFC-2018-v1.6",
                           rundir = tempdir()){
  # make the GFW grid and construct urls for intersecting tiles
  grid_GFC = .makeGFWGrid()
  tile_ids = st_intersects(st_as_sfc(bbox), grid_GFC)[[1]]
  if(length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the GFW grid.")
  urls = sapply(tile_ids, function(n) .getGFWTileURL(grid_GFC[n,], vers, "treecover2000"))

  # start download in a temporal directory within tmpdir
  #rundir = tempfile(tmpdir = tmpdir)
  #dir.create(rundir)
  for (url in urls){
    download.file(url, file.path(rundir, basename(url)))
    #command = sprintf("gdal_translate %s %s -of COG -co COMPRESS=LZW", url, file.path(rundir, basename(url)))
    #system(command)
  }
  # return all paths to the downloaded files
  list.files(rundir, full.names = T)
}


.makeGFWGrid <- function(xmin=-180, xmax=170, dx=10, ymin=-50, ymax=80, dy=10,
                        proj=NULL) {
  if (is.null(proj)) proj = st_crs(4326)
  ncells = c((xmax - xmin) / dx,
             (ymax - ymin) / dy)

  bbox = st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin))
  st_as_sf(st_make_grid(bbox, cellsize = 10, n = ncells, crs = "EPSG:4326", what = "polygons"))

}

.getGFWTileURL <- function(tile, vers, parameter){
  min_x = st_bbox(tile)[1]
  max_y = st_bbox(tile)[4]

  # prepare tile names
  if (min_x < 0) {
    min_x = paste0(sprintf('%03i', abs(min_x)), 'W')
  } else {
    min_x = paste0(sprintf('%03i', min_x), 'E')
  }
  if (max_y < 0) {
    max_y = paste0(sprintf('%02i', abs(max_y)), 'S')
  } else {
    max_y = paste0(sprintf('%02i', max_y), 'N')
  }

  baseurl = paste0("https://storage.googleapis.com/earthenginepartners-hansen/", vers, "/")
  filename = paste0("Hansen_", vers, "_", parameter, "_", max_y, "_", min_x, ".tif")
  url = paste0(baseurl, filename)
  url

}
