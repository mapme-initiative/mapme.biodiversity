#' Downloads Copernicus Global Land Cover layer
#'
#' The resource represents the actual surface cover of ground available
#' annually for the period 2015 to 2019. The cell values range from 0 to
#' 200, representing total of 23 discrete classifications from ESA.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @name ESA_Global_Land_Cover
#' @keywords internal
#'

.get_ESALandCover <- function(x,
                              rundir = tempdir(),
                              verbose = TRUE) {
  bbox <- st_bbox(x)
  target_years <- attributes(x)$years

  # make the ESA grid and construct urls for intersecting tiles
  grid_esa <- .makeESAGrid()
  tile_ids <- st_intersects(st_as_sfc(bbox), grid_esa)[[1]]
  if (length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the Land Cover grid.")
  urls <- unlist(mapply(.getEsaURL, grid_esa[tile_ids, ], target_years))

  # start download in a temporal directory within tmpdir
  if (verbose) pb <- progress_bar$new(total = length(urls))
  for (url in urls) {
    tryCatch(
      {
        if (verbose) pb$tick(0)
        download.file(url, file.path(rundir, basename(url)), quiet = TRUE)
        if (verbose) pb$tick()
      },
      error = function(e) {
        message("reading URLs!")
      }
    )
  }

  # return all paths to the downloaded files
  list.files(rundir, full.names = T)
}


.makeESAGrid <- function(xmin = -180, xmax = 180, dx = 20, ymin = -60, ymax = 80, dy = 20,
                         proj = NULL) {
  if (is.null(proj)) proj <- st_crs(4326)
  ncells <- c(
    (xmax - xmin) / dx,
    (ymax - ymin) / dy
  )

  bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin))
  st_as_sf(st_make_grid(bbox, cellsize = 20, n = ncells, crs = "EPSG:4326", what = "polygons"))
}


.getEsaURL <- function(tile, year) {
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
      url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-base_Discrete-Classification-map_EPSG-4326.tif")
      url
    } else if (year == 2019) {
      url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-nrt_Discrete-Classification-map_EPSG-4326.tif")
      url
    } else {
      url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-conso_Discrete-Classification-map_EPSG-4326.tif")
      url
    }
  } else {
    warning(sprintf("Copernicus land cover not available for target year %s", year))
    NULL
  }
}

