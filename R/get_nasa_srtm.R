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
#' @returns A function that returns a character of file paths.
#' @references NASA JPL (2020). NASADEM Merged DEM Global 1 arc second V001.
#'   NASA EOSDIS Land Processes DAAC. Accessed 2023-07-01 from
#'   https://doi.org/10.5067/MEaSUREs/NASADEM/NASADEM_HGT.001
#' @source https://planetarycomputer.microsoft.com/dataset/nasadem
#' @include register.R
#' @export
get_nasa_srtm <- function() {
  check_namespace("rstac")

  function(x,
           name = "nasa_srtm",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    urls <- try(rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
      rstac::stac_search(
        collection = "nasadem",
        bbox = as.numeric(st_bbox(x)),
        limit = NULL
      ) %>%
      rstac::post_request() %>%
      rstac::items_fetch() %>%
      rstac::assets_url(asset_names = "elevation"))

    if (inherits(urls, "try-error")) {
      stop("Download for NASA SRTM resource was unsuccesfull")
    }

    if (length(urls) == 0) {
      stop("The extent of the portfolio does not intersect with the SRTM grid.")
    }

    if (testing) {
      return(basename(urls))
    }

    filenames <- file.path(outdir, basename(urls))
    download_or_skip(urls = urls, filenames = filenames, check_existence = FALSE)
    filenames
  }
}

register_resource(
  name = "nasa_srtm",
  description = "NASA Shuttle Radar Topography Mission (SRTM) Digital Elevation Model (DEM)",
  licence = "https://lpdaac.usgs.gov/data/data-citation-and-policies/",
  source = "https://planetarycomputer.microsoft.com/dataset/nasadem",
  type = "raster"
)
