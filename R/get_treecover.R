#' Treecover for the year 2000
#'
#' This resource is part of the publication by Hansen et al. (2013)
#' "High-Resolution Global Maps of 21st-Century Forest Cover Change". It
#' represents "tree cover in the year 2000, defined as canopy closure for all
#' vegetation taller than 5m in height. Encoded as a percentage per output grid
#' cell, in the range 0–100." Due to changes in the satellites products used
#' in the compilation of the treecover product, results before the year 2011
#' and afterwards are not directly comparable until reprocessing has finished.
#' Users should be aware of this limitation, especially when the timeframe
#' of the analysis spans over the two periods delimited by the year 2011.
#'
#' The following argument can be set:
#' \describe{
#'   \item{vers_treecover}{The version of the dataset to download. Defaults to
#'   "GFC-2020-v1.8". Check mapme.biodiversity:::.available_gfw_versions()
#'   to get a list of available versions}
#' }
#'
#' @name treecover2000
#' @docType data
#' @keywords resource
#' @format A global tiled raster resource available for all land areas.
#' @references Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A.
#' Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R.
#' Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G.
#' Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover
#' Change.” Science 342 (15 November): 850–53.
#' @source \url{https://data.globalforestwatch.org/documents/tree-cover-2000/explore}
NULL


#' Get treecover layer
#'
#' @param x An sf object returned by init_portfolio
#' @param vers_treecover The version to download, defaults to
#'   \code{"GFC-2020-v1.8"}.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @keywords internal
#' @noRd
#'
.get_treecover <- function(x,
                           vers_treecover = "GFC-2020-v1.8",
                           rundir = tempdir(),
                           verbose = TRUE) {

  # check that version is correct
  if (!vers_treecover %in% .available_gfw_versions()) {
    stop(
      sprintf(
        "Wrong version specified for treecover resource. Select one of %s.",
        paste(.available_gfw_versions(), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  # make the GFW grid and construct urls for intersecting tiles
  bbox <- st_bbox(x)
  baseurl <- sprintf(
    "https://storage.googleapis.com/earthenginepartners-hansen/%s/",
    vers_treecover
  )
  grid_gfc <- .make_global_grid(
    xmin = -180, xmax = 170, dx = 10,
    ymin = -50, ymax = 80, dy = 10
  )
  tile_ids <- st_intersects(st_as_sfc(bbox), grid_gfc)[[1]]
  if (length(tile_ids) == 0) {
    stop("The extent of the portfolio does not intersect with the GFW grid.",
      call. = FALSE
    )
  }
  ids <- sapply(tile_ids, function(n) .get_gfw_tile_id(grid_gfc[n, ]))
  urls <- sprintf(
    "%sHansen_%s_treecover2000_%s.tif",
    baseurl, vers_treecover, ids
  )
  filenames <- file.path(rundir, basename(urls))
  if (attr(x, "testing")) {
    return(basename(filenames))
  }
  # start download and skip files that exist
  # TODO: parallel downloads
  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(urls, filenames, verbose, check_existence = FALSE, aria_bin = aria_bin)
  # return all paths to the downloaded files
  filenames
}

.available_gfw_versions <- function() {
  c(
    "GFC-2015-v1.3", "GFC-2016-v1.4", "GFC-2017-v1.5",
    "GFC-2018-v1.6", "GFC-2019-v1.7", "GFC-2020-v1.8"
  )
}
