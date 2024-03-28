#' Treecover for the year 2000
#'
#' This resource is part of the publication by Hansen et al. (2013)
#' represents "tree cover in the year 2000, defined as canopy closure for all
#' vegetation taller than 5m in height. Encoded as a percentage per output grid
#' cell, in the range 0–100." Due to changes in the satellites products used
#' in the compilation of the treecover product, results before the year 2011
#' and afterwards are not directly comparable until reprocessing has finished.
#' Users should be aware of this limitation, especially when the timeframe
#' of the analysis spans over the two periods delimited by the year 2011.
#
#' @name gfw_treecover
#' @param version The version of the dataset to download. Defaults to
#'   "GFC-2022-v1.10". Check mapme.biodiversity:::.available_gfw_versions()
#'   to get a list of available versions
#' @keywords resource
#' @returns A function that returns a character of file paths.
#' @references Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A.
#' Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R.
#' Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G.
#' Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover
#' Change.” Science 342 (15 November): 850–53.
#' @source \url{https://data.globalforestwatch.org/documents/tree-cover-2000/explore}
#' @include register.R
#' @export
get_gfw_treecover <- function(version = "GFC-2022-v1.10") {
  version <- .check_gfw_version(version)

  function(x,
           name = "gfw_treecover",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    # make the GFW grid and construct urls for intersecting tiles
    baseurl <- sprintf(
      "https://storage.googleapis.com/earthenginepartners-hansen/%s/",
      version
    )
    grid_gfc <- make_global_grid(
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
    urls <- sprintf(
      "%sHansen_%s_treecover2000_%s.tif",
      baseurl, version, ids
    )
    filenames <- file.path(outdir, basename(urls))
    if (mapme_options()[["testing"]]) {
      return(basename(filenames))
    }
    filenames <- download_or_skip(urls, filenames, check_existence = FALSE)
    filenames
  }
}

.available_gfw_versions <- function() {
  c(
    "GFC-2015-v1.3", "GFC-2016-v1.4", "GFC-2017-v1.5",
    "GFC-2018-v1.6", "GFC-2019-v1.7", "GFC-2020-v1.8",
    "GFC-2021-v1.9", "GFC-2022-v1.10"
  )
}

.check_gfw_version <- function(version) {
  if (!version %in% .available_gfw_versions()) {
    stop(
      sprintf(
        "Wrong version specified for treecover resource. Select one of %s.",
        paste(.available_gfw_versions(), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(version)
}

.get_gfw_tile_id <- function(tile) {
  min_x <- st_bbox(tile)[1]
  max_y <- st_bbox(tile)[4]

  # prepare tile names
  if (min_x < 0) {
    min_x <- paste0(sprintf("%03i", abs(min_x)), "W")
  } else {
    min_x <- paste0(sprintf("%03i", min_x), "E")
  }
  if (max_y < 0) {
    max_y <- paste0(sprintf("%02i", abs(max_y)), "S")
  } else {
    max_y <- paste0(sprintf("%02i", max_y), "N")
  }

  paste0(max_y, "_", min_x)
}


register_resource(
  name = "gfw_treecover",
  description = "Global Forest Watch - Percentage of canopy closure in 2000",
  licence = "CC-BY 4.0",
  source = "https://data.globalforestwatch.org/documents/tree-cover-2000/explore",
  type = "raster"
)
