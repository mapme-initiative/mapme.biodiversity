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
#' @references © European Union, Copernicus Land Monitoring Service (year),
#' European Environment Agency (EEA)", f.ex. in 2018: “© European Union,
#' Copernicus Land Monitoring Service 2018, European Environment Agency (EEA)
#' @source \url{https://www.nazka.be/en/offline}
#' @include register.R
#' @export
get_esalandcover <- function(years = 2015:2019) {
  years <- check_available_years(years, c(2015:2019), "esalandcover")

  function(x,
           name = "esalandcover",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    # make the ESA grid and construct urls for intersecting tiles
    grid_esa <- make_global_grid(
      xmin = -180, xmax = 180, dx = 20,
      ymin = -60, ymax = 80, dy = 20
    )
    tile_ids <- unique(unlist(st_intersects(x, grid_esa)))
    if (length(tile_ids) == 0) {
      stop(paste("The extent of the portfolio does not ",
        "intersect with the Land Cover grid.",
        sep = ""
      ))
    }
    # create all urls for target years and per tile
    fps <- purrr::map(tile_ids, function(id) {
      urls <- purrr::map_chr(years, .get_esa_url, tile = grid_esa[id, ])
      fp <- st_as_sf(rep(st_as_sfc(grid_esa[id, ]), length(urls)))
      fp[["source"]] <- urls

      filenames <- purrr::map_chr(basename(urls), function(x) {
        x <- strsplit(x, "-|_")[[1]]
        paste0(x[1], "_", x[3], "_", x[5], "_", x[6], ".tif")
      })

      fp[["filename"]] <- filenames
      fp
    })

    fps <- st_as_sf(purrr::list_rbind(fps))
    co <- c("-of", "COG", "-co", "COMPRESS=DEFLATE")
    fps <- make_footprints(fps, filenames = fps[["filename"]], what = "raster", co = co)
    does_exist <- purrr::map_lgl(fps[["source"]], spds_exists, what = "raster")
    fps <- fps[does_exist, ]
    if (length(fps) == 0) {
      return(NULL)
    }
    fps
  }
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
        "/vsicurl/https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",
        year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year,
        "-base_Discrete-Classification-map_EPSG-4326.tif"
      )
    } else if (year == 2019) {
      paste0(
        "/vsicurl/https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",
        year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year,
        "-nrt_Discrete-Classification-map_EPSG-4326.tif"
      )
    } else {
      paste0(
        "/vsicurl/https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",
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


register_resource(
  name = "esalandcover",
  licence = "CC-BY 4.0",
  description = "Copernicus Land Monitoring Service (CLMS) 100 meter land cover product",
  source = "https://registry.opendata.aws/esa-worldcover-vito/",
  type = "raster"
)
