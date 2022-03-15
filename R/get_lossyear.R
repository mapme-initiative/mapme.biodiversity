#' Downloads Year of Gross Forest Loss Event
#'
#' The layer is defined as forest loss during the period 2000–2019, defined as
#' a stand-replacement disturbance, or a change from a forest to non-forest
#' state. Encoded as either 0 (no loss) or else a value in the range 1–19,
#' representing loss detected primarily in the year 2001–2019, respectively.
#'
#' @param x An sf object returned by init_portfolio
#' @param vers_lossyear The version to download, defaults to
#'   \code{"GFC-2020-v1.8"}.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @name Year_of_Gross_Forest_Loss
#' @keywords internal
#'
.get_lossyear <- function(x,
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
  bbox <- st_bbox(x)
  baseurl <- sprintf(
    "https://storage.googleapis.com/earthenginepartners-hansen/%s/",
    vers_lossyear
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
  urls <- sprintf("%sHansen_%s_lossyear_%s.tif", baseurl, vers_lossyear, ids)
  filenames <- file.path(rundir, basename(urls))
  if (any(file.exists(filenames))) {
    message("Skipping existing files in output directory.")
  }
  # start download in a temporal directory within tmpdir
  # TODO: Parallel downloads
  .download_or_skip(urls, filenames, verbose)
  # return all paths to the downloaded files
  filenames
}
