#' Year of forest loss occurrence
#'
#' This resource is part of the publication by Hansen et al. (2013)
#' "High-Resolution Global Maps of 21st-Century Forest Cover Change". It
#' represents "Forest loss during the period 2000–2021, defined as a
#' stand-replacement disturbance, or a change from a forest to non-forest state.
#' Encoded as either 0 (no loss) or else a value in the range 1–20, representing
#' loss detected primarily in the year 2001–2021, respectively." Due to changes
#' in the satellites products used in the compilation of the tree loss product,
#' results before the year 2011 and afterwards are not directly comparable
#' until reprocessing has finished. Users should be aware of this limitation,
#'  especially when the timeframe of the analysis spans over the two periods
#'  delimited by the year 2011.
#'
#' @name gfw_lossyear
#' @param version The version of the dataset to download. Defaults to
#'   "GFC-2023-v1.11". Check \code{mapme.biodiversity:::.available_gfw_versions()}
#'   to get a list of available versions
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A.
#' Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R.
#' Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G.
#' Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover
#' Change.” Science 342 (15 November): 850–53.
#' @source \url{https://data.globalforestwatch.org/documents/tree-cover-loss/explore}
#' @include register.R
#' @export
get_gfw_lossyear <- function(version = "GFC-2023-v1.11") {
  version <- .check_gfw_version(version)

  function(x,
           name = "gfw_lossyear",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    # make the GFW grid and construct urls for intersecting tiles
    baseurl <- sprintf(
      "/vsicurl/https://storage.googleapis.com/earthenginepartners-hansen/%s/",
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
    urls <- sprintf("%sHansen_%s_lossyear_%s.tif", baseurl, version, ids)
    fps <- grid_gfc[tile_ids, ]
    fps[["source"]] <- urls
    make_footprints(fps,
      what = "raster",
      co = c("-co", "INTERLEAVE=BAND", "-co", "COMPRESS=LZW", "-ot", "Byte")
    )
  }
}

register_resource(
  name = "gfw_lossyear",
  description = "Global Forest Watch - Year of forest cover loss occurrence",
  licence = "CC-BY 4.0",
  source = "https://data.globalforestwatch.org/documents/tree-cover-loss/explore",
  type = "raster"
)
