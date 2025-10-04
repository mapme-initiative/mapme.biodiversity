#' ESA Copernicus Global Land Cover layer
#'
#' This 100 meter spatial resolution land cover resource is published by Buchhorn
#' et al. (2020) "Copernicus Global Land Cover Layers—Collection 2". The resource
#' represents the actual surface cover of ground available annually for the period
#' 2015 to 2019. The cell values range from 0 to 200, representing total of 23
#' discrete classifications from ESA.
#'
#' @name esalandcover
#' @param years A numeric vector indicating the years for which to make the
#'   resource available.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @details
#' This function mimics the behavior of the original `get_esalandcover()` function by
#' downloading the parts of the global raster that correspond to the 20° x 20° tiles used
#' previously. The files are named the same as before. This allows other code that uses
#' this resource to remain unchanged.
#' @references Buchhorn, M., Lesiv, M., Tsendbazar, N.-E., Herold, M., Bertels, L.,
#' & Smets, B. (2020). Copernicus Global Land Cover Layers - Collection 2.
#' Remote Sensing, 12(6), 1044.
#' \doi{doi:10.3390/rs12061044}
#' @source \url{https://zenodo.org/records/3939050}
#' @include register.R
#' @export
get_esalandcover <- function(years = 2015L:2019L) {
  years <- check_available_years(years, c(2015L:2019L), "esalandcover")

  function(x,
           name = "esalandcover",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {

    # make the ESA grid and construct urls for intersecting tiles
    grid_esa <- make_global_grid(
      xmin = -180.0, xmax = 180.0, dx = 20.0,
      ymin = -60.0, ymax = 80.0, dy = 20.0
    )
    # find requoired (intersecting) tiles
    tile_ids <- unique(unlist(sf::st_intersects(x, grid_esa)))
    if (length(tile_ids) == 0L) {
      stop(paste("The extent of the portfolio does not ",
        "intersect with the Land Cover grid.",
        sep = ""
      ))
    }
    # create all urls for target years and per tile
    fps <- purrr::map(tile_ids, function(id) {
      urls <- purrr::map_chr(years, .get_esa_url)
      filenames <- purrr::map_chr(years, .get_esa_filename, tile = grid_esa[id, ])
      # options will define the part of global raster to retrieve
      # options <- purrr::map(years, .get_esa_option, tile = grid_esa[id, ])
      options <- rep(.get_esa_option(tile = grid_esa[id, ]), length.out = length(urls))
      fp <- sf::st_as_sf(rep(sf::st_as_sfc(grid_esa[id, ]), length(urls)))
      fp[["source"]] <- urls
      fp[["filename"]] <- filenames
      fp[["oo"]] <- options
      fp
    })

    fps <- sf::st_as_sf(purrr::list_rbind(fps))
    co <- c("-of", "COG", "-co", "COMPRESS=DEFLATE")

    fps <- make_footprints(fps, filenames = fps[["filename"]], what = "raster", co = co, oo = fps[["oo"]])
    if (length(fps) == 0L) {
      return(NULL)
    }
    fps
  }
}

#' Helper function to create ESA land cover urls
#'
#' @param year A single numeric value indicating the target year
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_esa_url <- function(year) {
  urls <- c(
    "/vsicurl/https://zenodo.org/records/3939038/files/PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.tif",
    "/vsicurl/https://zenodo.org/records/3518026/files/PROBAV_LC100_global_v3.0.1_2016-conso_Discrete-Classification-map_EPSG-4326.tif",
    "/vsicurl/https://zenodo.org/records/3518036/files/PROBAV_LC100_global_v3.0.1_2017-conso_Discrete-Classification-map_EPSG-4326.tif",
    "/vsicurl/https://zenodo.org/records/3518038/files/PROBAV_LC100_global_v3.0.1_2018-conso_Discrete-Classification-map_EPSG-4326.tif",
    "/vsicurl/https://zenodo.org/records/3939050/files/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"
  )
  return(urls[match(year, 2015L:2019L)])
}

#' Helper function to create ESA land cover file names
#'
#' @param tile An sf object representing the spatial extent of the a tile
#' @param year A single numeric value indicating the target year
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_esa_filename <- function(tile, year) {
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
  filename <- sprintf("%s_LC100_v3.0.1_%s.tif", grid, year)
  return(filename)
}

#' Helper function to create ESA land cover GDAL options
#'
#' @param tile An sf object representing the spatial extent of the a tile
#'
#' @return A character vector
#' @keywords internal
#' @noRd
.get_esa_option <- function(tile) {
  bb <- sf::st_bbox(tile)
  ext <- sprintf("%f", as.numeric(bb[c("xmin", "ymax", "xmax", "ymin")]))
  opt <- c("-projwin", ext)
  return(list(opt))
}

register_resource(
  name = "esalandcover",
  licence = "CC-BY 4.0",
  description = "Copernicus Land Monitoring Service (CLMS) 100 meter land cover product",
  source = "https://registry.opendata.aws/esa-worldcover-vito/",
  type = "raster"
)
