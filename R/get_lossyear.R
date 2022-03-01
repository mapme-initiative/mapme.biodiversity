#' Downloads Year of Gross Forest Loss Event
#'
#' The layer is defined as forest loss during the period 2000–2019, defined as
#' a stand-replacement disturbance, or a change from a forest to non-forest state.
#' Encoded as either 0 (no loss) or else a value in the range 1–17, representing
#' loss detected primarily in the year 2001–2019, respectively.
#'
#' @param x An sf object returned by init_portfolio
#' @param vers_lossyear The version to download, defaults to \code{"GFC-2020-v1.8"}.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @name Year_of_Gross_Forest_Loss
#' @keywords internal
#'
.get_lossyear <- function(x,
                          vers_lossyear = "GFC-2020-v1.8",
                          rundir = tempdir(),
                          verbose = TRUE){
  # check that version is correct
  if(!vers_lossyear %in% .available_GFW_versions()){
    stop(sprintf("Wrong version specified for lossyear resource. Select one of %s.", .available_GFW_versions()))
  }
  # make the GFW grid and construct urls for intersecting tiles
  bbox = st_bbox(x)
  baseurl = sprintf("https://storage.googleapis.com/earthenginepartners-hansen/%s/", vers_lossyear)
  grid_GFC = .makeGFWGrid()
  tile_ids = st_intersects(st_as_sfc(bbox), grid_GFC)[[1]]
  if(length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the GFW grid.", call. = FALSE)
  ids = sapply(tile_ids, function(n) .getGFWTileId(grid_GFC[n,]))
  urls = sprintf("%sHansen_%s_lossyear_%s.tif", baseurl, vers_lossyear, ids)

  # start download in a temporal directory within tmpdir
  # TODO: Parallel downloads
  if(verbose) pb = progress::progress_bar$new(total = length(urls))
  if(verbose) pb$tick(0)
  for (url in urls){
    download.file(url, file.path(rundir, basename(url)), quiet = TRUE)
    if(verbose) pb$tick()
  }
  # return all paths to the downloaded files
  list.files(rundir, full.names = T)
}
