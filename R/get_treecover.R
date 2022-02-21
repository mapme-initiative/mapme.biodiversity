.get_treecover <- function(x,
                           vers_treecover = "GFC-2018-v1.6",
                           rundir = tempdir(),
                           verbose = TRUE){

  # TODO: check that version is correct
  # make the GFW grid and construct urls for intersecting tiles
  bbox = st_bbox(x)
  baseurl = sprintf("https://storage.googleapis.com/earthenginepartners-hansen/%s/", vers_treecover)
  grid_GFC = .makeGFWGrid()
  tile_ids = st_intersects(st_as_sfc(bbox), grid_GFC)[[1]]
  if(length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the GFW grid.", call. = FALSE)
  ids = sapply(tile_ids, function(n) .getGFWTileId(grid_GFC[n,]))
  urls = sprintf("%sHansen_%s_treecover2000_%s.tif", baseurl, vers_treecover, ids)

  #start download in a temporal directory within tmpdir
  # TODO: parallel downloads
  if(verbose) pb = progress_bar$new(total = length(urls))
  for (url in urls){
    if(verbose) pb$tick(0)
    download.file(url, file.path(rundir, basename(url)), quiet = TRUE)
    if(verbose) pb$tick()
  }
  # return all paths to the downloaded files
  list.files(rundir, full.names = T)
}
