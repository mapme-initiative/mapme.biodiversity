#' Population Count layer for year 2000-2020
#'
#' This resource is published by open spatial demographic data and research
#' organization called WorldPop. This resource represents the population
#' count, 1 km spatial resolution layers available to download from the year
#' 2000 to 2020. The dataset is called as WorldPop Unconstrained Global Mosaics.
#' The encoded cell value represents the total number of people in that particular
#' grid cell.
#'
#' @name worldpop
#' @docType data
#' @keywords resource
#' @format Global raster layers available for years 2000-2020.
#' @source \url{https://www.worldpop.org/geodata/listing?id=64}
NULL


#' Downloads Population Count layer
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @keywords internal
#' @noRd

.get_worldpop <- function(x,
                          rundir = tempdir(),
                          verbose = TRUE) {
  target_years <- attributes(x)$years
  available_years <- 2000:2020
  target_years <- .check_available_years(
    target_years, available_years, "popcount"
  )
  urls <- unlist(sapply(target_years, function(year) .get_worldpop_url(year)))
  filenames <- file.path(rundir, basename(urls))
  # start download in a temporal directory within tmpdir
  aria_bin <- attributes(x)$aria_bin
  if (is.null(attr(x, "testing"))) .download_or_skip(urls, filenames, verbose, aria_bin = aria_bin, check_existence = FALSE)

  footprints <- lapply(filenames, function(file) {
    paste(as.character(st_bbox(rast(file))), collapse = " ")
  })
  all_equal <- length(unique(unlist(footprints))) == 1

  if (!all_equal) {
    footprints <- unlist(lapply(filenames, function(file) {
      bbox <- round(as.numeric(st_bbox(rast(file))), 3)
      identical(bbox, round(c(-180.00125, -72.00042, 179.99875, 83.99958), 3))
    }))
    if (any(!footprints)) {
      index <- which(!footprints)
      index_ok <- which(footprints)[1]
      target <- rast(filenames[index_ok])
      if (verbose) message("Resampling worldpop layers...")
      for (i in index) {
        tmpfile <- tempfile(tmpdir = rundir, fileext = ".tif")
        file.copy(filenames[i], tmpfile)
        tmp <- rast(tmpfile)
        project(tmp, target,
          filename = filenames[i],
          overwrite = TRUE, method = "bilinear", progress = verbose
        )
        file.remove(tmpfile)
      }
    }
  }
  # return paths to the raster
  filenames
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
