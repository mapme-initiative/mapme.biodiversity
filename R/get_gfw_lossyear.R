#' Year of forest loss occurrence
#'
#' This resource is part of the publication by Hansen et al. (2013)
#' "High-Resolution Global Maps of 21st-Century Forest Cover Change". It
#' represents "Forest loss during the period 2000–2020, defined as a
#' stand-replacement disturbance, or a change from a forest to non-forest state.
#' Encoded as either 0 (no loss) or else a value in the range 1–20, representing
#' loss detected primarily in the year 2001–2020, respectively." Due to changes
#' in the satellites products used in the compilation of the tree loss product,
#' results before the year 2011 and afterwards are not directly comparable
#' until reprocessing has finished. Users should be aware of this limitation,
#'  especially when the timeframe of the analysis spans over the two periods
#'  delimited by the year 2011.
#'
#' The following argument can be set:
#' \describe{
#'   \item{vers_lossyear}{The version of the dataset to download. Defaults to
#'   "GFC-2020-v1.8". Check \code{mapme.biodiversity:::.available_gfw_versions()}
#'   to get a list of available versions}
#' }
#'
#' @name gfw_lossyear
#' @docType data
#' @keywords resource
#' @format A global tiled raster resource available for all land areas.
#' @references Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A.
#' Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R.
#' Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G.
#' Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover
#' Change.” Science 342 (15 November): 850–53.
#' @source \url{https://data.globalforestwatch.org/documents/tree-cover-loss/explore}
NULL

#' Get lossyear layer
#'
#' @param x An sf object returned by init_portfolio
#' @param vers_lossyear The version to download, defaults to
#'   \code{"GFC-2020-v1.8"}.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @keywords internal
#' @noRd
.get_gfw_lossyear <- function(x,
                              vers_lossyear = "GFC-2020-v1.8",
                              rundir = tempdir(),
                              verbose = TRUE) {
  # check that version is correct
  if (!vers_lossyear %in% .available_gfw_versions()) {
    stop(
      sprintf(
        paste("Wrong version specified for lossyear resource. ",
          "Select one of %s.",
          sep = ""
        ), .available_gfw_versions()
      ),
      call. = FALSE
    )
  }
  # make the GFW grid and construct urls for intersecting tiles
  baseurl <- sprintf(
    "https://storage.googleapis.com/earthenginepartners-hansen/%s/",
    vers_lossyear
  )
  grid_gfc <- .make_global_grid(
    xmin = -180, xmax = 170, dx = 10,
    ymin = -50, ymax = 80, dy = 10
  )
  tile_ids <- unique(unlist(st_intersects(x, grid_gfc)))
  if (length(tile_ids) == 0) {
    stop("The extent of the portfolio does not intersect with the GFW grid.",
      call. = FALSE
    )
  }
  ids <- sapply(tile_ids, function(n) .get_gfw_tile_id(grid_gfc[n, ]))
  urls <- sprintf("%sHansen_%s_lossyear_%s.tif", baseurl, vers_lossyear, ids)
  filenames <- file.path(rundir, basename(urls))
  if (attr(x, "testing")) {
    return(basename(filenames))
  }
  # start download in a temporal directory within tmpdir
  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(urls, filenames, verbose, check_existence = FALSE, aria_bin = aria_bin)
  # return all paths to the downloaded files
  filenames
}
