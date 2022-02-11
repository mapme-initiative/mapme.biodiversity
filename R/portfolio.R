#' Conversion method for sf objects
#'
#' Checks to be performed:
#' - has at least one asset
#' - all assets are either Polygon or MultiPolygon (TODO)
#' - is Lat/Lon, otherwise transform to EPSG:4326
#'
#' @param x The sf object to be transformed to a portfolio
#' @param years Numeric vector for the analysis years
#' @param outdir Character indicating the directory where resources are written
#' @param tmpdir Character vector to a directory used for intermediate files.
#'   Defaults to the output of tempdir(). Should not be identical with outdir.
#' @export
init_portfolio = function(x, years, outdir, tmpdir = tempdir()){
  if(outdir == tmpdir) stop("Parameters outdir and tmpdir need to point to different directories.")
  if(!file.exists(outdir)) dir.create(outdir)
  if(!file.exists(tmpdir)) dir.create(tmpdir)
  if(nrow(x)<1) stop("x must contain at least one asset.")
  if(st_crs(x) != st_crs(4326)){
    message("CRS of x is not EPSG:4326. Attempting to transform.")
    x = st_transform(x, 4326)
  }
  x = st_as_sf(tibble(x))
  #setting portfolio level attributes
  attr(x, "nitems") =  nrow(x)
  attr(x, "bbox") = st_bbox(x)
  # todo: think about ways how users can specify a sources directory
  attr(x, "sources") = NA
  attr(x, "years") = years
  attr(x, "outdir") = outdir
  attr(x, "tmpdir") = tmpdir
  x
}
