#' Population Count layer for year 2000-2020
#'
#' This resource is published by open spatial demographic data and research
#' organization called WorldPop. This resource represents the population
#' count, 1 km spatial resolution layers available to download from the year
#' 2000 to 2020. The dataset is called as WorldPop Unconstrained Global Mosaics.
#' The encoded cell value represents the total number of people in that particular
#' grid cell.
#'
#' It may be required to increase the timeout option to successfully download
#' theses WorldPop layers from their source location via e.g.
#' `options(timeout = 600)`.
#'
#'
#' @name worldpop
#' @param years A numeric vector indicating the years for which to make the
#'   resource available.
#' @docType data
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @source \url{https://www.worldpop.org/}
#' @include register.R
#' @export
get_worldpop <- function(years = 2000) {
  years <- check_available_years(years, c(2000:2020), "worldpop")

  if (is.null(mapme_options()[["outdir"]])) {
    warning(paste(
      "Worldpop layers must be downloaded from the source location",
      "irrespective if `outdir` was specified or not."
    ))
  }

  function(x,
           name = "worldpop",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    srcs <- unlist(sapply(years, function(year) .get_worldpop_url(year)))
    has_outdir <- !is.null(outdir)

    if (has_outdir) {
      dsts <- file.path(outdir, basename(srcs))
    } else {
      tmpdir <- tempfile()
      dir.create(tmpdir)
      dsts <- file.path(tmpdir, basename(srcs))
    }

    is_available <- purrr::map_lgl(dsts, spds_exists, what = "raster")
    if (all(is_available)) {
      return(make_footprints(dsts, what = "raster"))
    }

    purrr::walk2(srcs, dsts, function(src, dst) {
      if (!spds_exists(dst, what = "raster")) {
        # download original worldpop layer
        tmp <- tempfile(fileext = ".tif")
        download.file(src, tmp, mode = ifelse(Sys.info()["sysname"] == "Windows", "wb", "w"))

        # translate to regular grid
        sf::gdal_utils(
          util = "translate",
          source = tmp,
          destination = dst,
          options = c(
            "-co", "COMPRESS=LZW",
            "-co", "PREDICTOR=2",
            "-ot", "Float32",
            "-a_ullr", "-180.0", "84.0", "180.0", "-72.0"
          )
        )
        file.remove(tmp)
      }
    })

    make_footprints(dsts, what = "raster")
  }
}

#' Helper function to construct population layer urls
#'
#' @param target_year A numeric indicating the target year
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
.get_worldpop_url <- function(target_year) {
  available_years <- c(2000:2020)
  if (target_year %in% available_years) {
    paste0(
      "https://data.worldpop.org/GIS/Population/Global_2000_2020/",
      target_year, "/0_Mosaicked/ppp_", target_year, "_1km_Aggregated.tif"
    )
  } else {
    warning(
      sprintf(
        "Population count not available for target year %s", target_year
      )
    )
    NULL
  }
}


register_resource(
  name = "worldpop",
  description = "WorldPop - Unconstrained Global Mosaics 2000 - 2020",
  licence = "CC-BY 4.0",
  source = "https://www.worldpop.org/",
  type = "raster"
)
