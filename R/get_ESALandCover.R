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
  available_years = c(2015:2019)
  target_years = .check_available_years(target_years, available_years, "esalandcover")

  # make the ESA grid and construct urls for intersecting tiles
  grid_esa <- .makeGlobalGrid(xmin = -180, xmax = 180, dx = 20, ymin = -60, ymax = 80, dy = 20)
  tile_ids <- st_intersects(st_as_sfc(bbox), grid_esa)[[1]]
  if (length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the Land Cover grid.")
  # create all urls for target years and per tile
  urls <- lapply(target_years, function(year){
    out = vector(length = length(tile_ids))
    for(i in 1:length(tile_ids)){
      out[i] = .getEsaURL(grid_esa[tile_ids[i], ], year)
    }
    out
  })
  # urls to vector
  urls = unlist(urls)
  filenames =  file.path(rundir, basename(urls))
  if(any(file.exists(filenames))) message("Skipping existing files in output directory.")
  .downloadOrSkip(urls, filenames, verbose)
  # return all paths to the downloaded files
  list.files(rundir, full.names = T)
}

#' Helper function to create ESA land cover urls
#'
#' @param tile An sf object representing the spatial extent of the a tile
#' @param year A single numeric value indicating the target year
#'
#' @return A charchter vector
#' @keywords internal
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
    } else if (year == 2019) {
      url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-nrt_Discrete-Classification-map_EPSG-4326.tif")
    } else {
      url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-conso_Discrete-Classification-map_EPSG-4326.tif")
    }
  } else {
    warning(sprintf("Copernicus land cover not available for target year %s", year))
    return(NULL)
  }
  url
}

