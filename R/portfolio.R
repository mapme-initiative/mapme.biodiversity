#' Initialization of a biodiversity portfolio object
#'
#' This function expects an \code{sf} object as its first argument that contains
#' only geometry of type \code{POLYGON} or \code{MULTIPOLYGON}. Each row of the
#' object is considered a single asset in the portfolio for which biodiversity
#' indicators will be calculated further down the processing chain. Some
#' preliminary checks are conducted, e.g. that the CRS of the object is
#' EPSG:4326 otherwise it will be transformed. Some portfolio wide parameters
#' such as the output directory for downloaded data sets, a temporal directory
#' for intermediate calculation of the number of cores available for the
#' indicator calculation can be set by the user to have more fine-control of
#' the workflow. However, these parameters are also set to sensible defaults
#' and thus can be omitted during portfolio initialization.
#'
#' @param x The sf object to be transformed to a portfolio
#' @param years Numeric vector for time frame of the analysis handed over as a
#'   vector of consecutive years
#' @param outdir Character indicating the directory where resources will be
#'   written to. If the directory does not exist, we will attempt to create it.
#'   The output director cannot be equal to the temporary directory. Defaults
#'   to the current working directory.
#' @param tmpdir Character vector to a directory used for intermediate files.
#'   Defaults to the output of \code{tempdir()}, e.g. the current R session's
#'   temporal directory. If a custom file path does not exist, we will attempt
#'   to create it. Cannot be equal to the output directory.
#' @param cores An integer value indicating the number of cores on the host
#'   machine available for indicator calculation. It defaults to
#'   \code{parallel::detectCores() - 1} cores, i.e. one core less than all
#'   available cores.
#' @param verbose Logical, defaults to TRUE, indicating if progress information
#'   is printed.
#' @export
init_portfolio <- function(x,
                           years,
                           outdir = getwd(),
                           tmpdir = tempdir(),
                           cores = parallel::detectCores() - 1,
                           verbose = TRUE) {
  if (outdir == tmpdir) {
    stop("Parameters outdir and tmpdir need to point to different directories.")
  }
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
  if (nrow(x) < 1) stop("x must contain at least one asset.")
  if (st_crs(x) != st_crs(4326)) {
    message("CRS of x is not EPSG:4326. Attempting to transform.")
    x <- st_transform(x, 4326)
  }
  # use tibble for prettier printing of the object
  x <- st_as_sf(tibble(x))
  # check for geometry types
  if (any(!unique(st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Some assests are not of type POLYGON or MULTIPOLYGON.")
  }
  # add a unique asset identifier
  if (".assetid" %in% names(x)) {
    message(
      paste("Found a column named '.assetid'. ",
        "Overwritting its values with a unique identifier.",
        sep = ""
      ),
    )
  }
  x$.assetid <- 1:nrow(x)
  # setting portfolio level attributes
  attr(x, "nitems") <- nrow(x)
  attr(x, "bbox") <- st_bbox(x)
  # todo: think about ways how users can specify a
  # pre-existing sources directory
  attr(x, "resources") <- list()
  attr(x, "years") <- years
  attr(x, "outdir") <- outdir
  attr(x, "tmpdir") <- tmpdir
  attr(x, "cores") <- cores
  attr(x, "verbose") <- verbose
  x
}
