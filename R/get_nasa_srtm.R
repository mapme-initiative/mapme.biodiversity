#' NASADEM HGT v001
#'
#' This resource is processed by the Land Processes Distributed Active Archive
#' Center (LP DAAC) and made available at the Microsoft Planetery Computer.
#' NASADEM are distributed in 1 degree latitude by 1 degree longitude tiles and
#' consist of all land between 60° N and 56° S latitude. This accounts for about
#' 80% of Earth’s total landmass.
#'
#' @name nasa_srtm
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references NASA JPL (2020). NASADEM Merged DEM Global 1 arc second V001.
#'   NASA EOSDIS Land Processes DAAC. Accessed 2023-07-01 from
#'   \doi{https://doi.org/10.5067/MEaSUREs/NASADEM/NASADEM_HGT.001}
#' @source \url{https://planetarycomputer.microsoft.com/dataset/nasadem}
#' @include register.R
#' @export
get_nasa_srtm <- function() {
  check_namespace("rstac")

  function(x,
           name = "nasa_srtm",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    items <- try(rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
      rstac::stac_search(
        collection = "nasadem",
        bbox = as.numeric(st_bbox(x)),
        limit = NULL
      ) %>%
      rstac::post_request() %>%
      rstac::items_fetch())

    if (inherits(items, "try-error")) {
      stop("Download for NASA SRTM resource was unsuccesfull")
    }

    urls <- rstac::assets_url(items, asset_names = "elevation")
    if (length(urls) == 0) {
      stop("The extent of the portfolio does not intersect with the SRTM grid.")
    }

    bboxs <- rstac::items_bbox(items)
    fps <- purrr::map(bboxs, function(x) {
      names(x) <- c("xmin", "ymin", "xmax", "ymax")
      bbox <- st_bbox(x, crs = "EPSG:4326")
      st_as_sf(st_as_sfc(bbox))
    })
    fps <- st_as_sf(purrr::list_rbind(fps))
    fps[["source"]] <- paste0("/vsicurl?pc_url_signing=yes&url=", urls)
    make_footprints(fps, what = "raster", co = c("-co", "COMPRESS=DEFLATE"))
  }
}

register_resource(
  name = "nasa_srtm",
  description = "NASA Shuttle Radar Topography Mission (SRTM) Digital Elevation Model (DEM)",
  licence = "https://lpdaac.usgs.gov/data/data-citation-and-policies/",
  source = "https://planetarycomputer.microsoft.com/dataset/nasadem",
  type = "raster"
)
