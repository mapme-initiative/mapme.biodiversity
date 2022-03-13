#' Downloads Tree Cover 2000 layer
#'
#' This resource represents the tree cover in the year 2000, defined as canopy
#' closure for all vegetation taller than 5m in height. It is encoded as a
#' percentage per output grid cell in the range 0 to 100.
#'
#' @param x An sf object returned by init_portfolio
#' @param vers_treecover The version to download, defaults to
#'   \code{"GFC-2020-v1.8"}.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @name Forest_Treecover_2000
#' @keywords internal
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
        .available_gfw_versions()
      )
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
  if (any(file.exists(filenames))) {
    message("Skipping existing files in output directory.")
  }
  # start download and skip files that exist
  # TODO: parallel downloads
  .download_or_skip(urls, filenames, verbose)
  # return all paths to the downloaded files
  filenames
}

.available_gfw_versions <- function() {
  c(
    "GFC-2015-v1.3", "GFC-2016-v1.4", "GFC-2017-v1.5",
    "GFC-2018-v1.6", "GFC-2019-v1.7", "GFC-2020-v1.8"
  )
}
