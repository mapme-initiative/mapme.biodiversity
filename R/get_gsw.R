.get_gsw <- function(x, statistic = "occurrence", vers_gsw = "v1_4_2021",
                     rundir = tempdir(), verbose = TRUE) {
  stopifnot(statistic %in% .gsw_statistics,
            vers_gsw %in% .gsv_versions)

  # make the gsw grid and construct urls for intersecting tiles
  baseurl <- sprintf(
    "https://storage.googleapis.com/global-surface-water/downloads2021/%s/%s",
    statistic, statistic
  )
  grid_gfc <- .make_global_grid(
    xmin = -180, xmax = 170, dx = 10,
    ymin = -50, ymax = 80, dy = 10
  )
  tile_ids <- unique(unlist(st_intersects(x, grid_gfc)))
  if (length(tile_ids) == 0) {
    stop("The extent of the portfolio does not intersect with the GSW grid.",
         call. = FALSE
    )
  }
  ids <- sapply(tile_ids, function(n) .get_gsw_tile_id(grid_gfc[n, ]))
  urls <- sprintf(
    "%s_%s%s.tif",
    baseurl, ids, vers_gsw
  )
  filenames <- file.path(rundir, basename(urls))
  # start download and skip files that exist
  # TODO: parallel downloads
  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(urls, filenames, verbose, check_existence = FALSE,
                    aria_bin = aria_bin)
  # return all paths to the downloaded files
  filenames
}

.get_gsw_tile_id <- function(tile) {
  min_x <- st_bbox(tile)[1]
  max_y <- st_bbox(tile)[4]
  ndigits_x <- nchar(abs(min_x))
  ndigits_y <- nchar(abs(max_y))
  formatstr_x <- paste0("%0", ndigits_x, "i")
  formatstr_y <- paste0("%0", ndigits_y, "i")
  if (min_x < 0) {
    min_x <- paste0(sprintf(formatstr_x, abs(min_x)), "W")
  } else {
    min_x <- paste0(sprintf(formatstr_x, abs(min_x)), "E")
  }
  if (max_y < 0) {
    max_y <- paste0(sprintf(formatstr_y, abs(max_y)), "S")
  } else {
    max_y <- paste0(sprintf(formatstr_y, abs(max_y)), "N")
  }
  paste0(min_x, "_", max_y)
}

.gsw_statistics <- c(
  "change",
  "occurrence",
  "recurrence",
  "seasonality",
  "transitions"
)

.gsv_versions <- c(
  "v1_4_2021"
)

for (gsw_statistic in .gsw_statistics) {
  resource_name <- paste0("global_surface_water_", gsw_statistic)
  register_resource(
    name = resource_name,
    type = "raster",
    source = "https://global-surface-water.appspot.com/download",
    fun = .get_gsw,
    arguments = list(
      statistic = gsw_statistic,
      vers_gsw = "v1_4_2021"
    )
  )
}
