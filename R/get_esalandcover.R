#' ESA Copernicus Global Land Cover layer
#'
#' This 100 meter spatial resolution land cover resource is published by Buchhorn
#' et al. (2020) "Copernicus Global Land Cover Layers—Collection 2". The resource
#' represents the actual surface cover of ground available annually for the period
#' 2015 to 2019. The cell values range from 0 to 200, representing total of 23
#' discrete classifications from ESA.
#'
#' @name esalandcover
#' @docType data
#' @keywords resource
#' @format A global tiled raster resource available for years 2015 to 2019.
#' @references © European Union, Copernicus Land Monitoring Service (year),
#' European Environment Agency (EEA)", f.ex. in 2018: “© European Union,
#' Copernicus Land Monitoring Service 2018, European Environment Agency (EEA)
#' @source \url{https://lcviewer.vito.be/download}
NULL


#' Downloads Copernicus Global Land Cover layer
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @keywords internal
#' @noRd

.get_esalandcover <- function(x,
                              rundir = tempdir(),
                              verbose = TRUE) {
  bbox <- st_bbox(x)
  target_years <- attributes(x)$years
  available_years <- c(2015:2019)
  target_years <- .check_available_years(
    target_years, available_years, "esalandcover"
  )

  # make the ESA grid and construct urls for intersecting tiles
  grid_esa <- .make_global_grid(
    xmin = -180, xmax = 180, dx = 20,
    ymin = -60, ymax = 80, dy = 20
  )
  tile_ids <- st_intersects(st_as_sfc(bbox), grid_esa)[[1]]
  if (length(tile_ids) == 0) {
    stop(paste("The extent of the portfolio does not ",
      "intersect with the Land Cover grid.",
      sep = ""
    ))
  }
  # create all urls for target years and per tile
  urls <- lapply(target_years, function(year) {
    out <- vector(length = length(tile_ids))
    for (i in seq_along(tile_ids)) {
      out[i] <- .get_esa_url(grid_esa[tile_ids[i], ], year)
    }
    out
  })
  # urls to vector
  urls <- unlist(urls)
  # change filename structure
  bn <- basename(urls)
  chars <- strsplit(bn, "-|_")
  charname <- lapply(1:length(chars), function(j) {
    paste0(chars[[j]][1], "_", chars[[j]][3], "_", chars[[j]][5], "_", chars[[j]][6], ".tif")
  })
  charnames <- unlist(charname)
  filenames <- file.path(rundir, basename(charnames))
  aria_bin <- attributes(x)$aria_bin
  if (is.null(attr(x, "testing"))) {
    filenames <- .download_or_skip(urls,
      filenames,
      verbose,
      aria_bin = aria_bin,
      check_existence = TRUE
    )
  }
  # return all paths to the downloaded files
  filenames
}

#' Helper function to create ESA land cover urls
#'
#' @param tile An sf object representing the spatial extent of the a tile
#' @param year A single numeric value indicating the target year
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_esa_url <- function(tile, year) {
  min_x <- st_bbox(tile)[1]
  max_y <- st_bbox(tile)[4]

  # prepare tile names
  if (min_x < 0) {
    min_x <- paste0("W", sprintf("%03i", abs(min_x)))
  } else {
    min_x <- paste0("E", sprintf("%03i", min_x))
  }
  if (max_y < 0) {
    max_y <- paste0("S", sprintf("%02i", abs(max_y)))
  } else {
    max_y <- paste0("N", sprintf("%02i", max_y))
  }

  grid <- paste0(min_x, max_y)

  if (year %in% c(2015:2019)) {
    if (year == 2015) {
      paste0(
        "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",
        year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year,
        "-base_Discrete-Classification-map_EPSG-4326.tif"
      )
    } else if (year == 2019) {
      paste0(
        "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",
        year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year,
        "-nrt_Discrete-Classification-map_EPSG-4326.tif"
      )
    } else {
      paste0(
        "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",
        year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year,
        "-conso_Discrete-Classification-map_EPSG-4326.tif"
      )
    }
  } else {
    warning(sprintf(
      "Copernicus land cover not available for target year %s", year
    ))
    return(NULL)
  }
}
