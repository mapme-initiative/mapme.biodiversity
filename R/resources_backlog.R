#' Backlog function for available resources
#'
#' This function returns a list of either all available resources and some
#' additional metadata or for one or more requested resources. It can be used
#' by users of the package to inform themselves about the available data sets
#' and to learn about potentially additional arguments that should be specified
#' when requesting the resource.
#'
#' @param resources Defaults to NULL meaning that a list with all available
#'   resources will be returned. If a character vector is specified only the
#'   information about the requested resource will be returned.
#'
#' @return A list object.
#' @export
#' @keywords resource
#' @examples
#' names(available_resources())
available_resources <- function(resources = NULL) {
  all_resources <- list(
    gfw_treecover = list(
      type = "raster",
      source = "https://data.globalforestwatch.org/documents/tree-cover-2000/explore",
      downloader = ".get_gfw_treecover",
      arguments = list(
        vers_treecover = "GFC-2020-v1.8"
      )
    ),
    gfw_lossyear = list(
      type = "raster",
      source = "https://data.globalforestwatch.org/documents/tree-cover-loss/explore",
      downloader = ".get_gfw_lossyear",
      arguments = list(
        vers_lossyear = "GFC-2020-v1.8"
      )
    ),
    gfw_emissions = list(
      "type" = "raster",
      source = "https://data.globalforestwatch.org/datasets/gfw::forest-greenhouse-gas-emissions/about",
      downloader = ".get_gfw_emissions",
      arguments = list()
    ),
    worldpop = list(
      type = "raster",
      source = "WorldPop",
      downloader = ".get_worldpop",
      arguments = list()
    ),
    esalandcover = list(
      type = "raster",
      source = "Copernicus (ESA)",
      downloader = ".get_esalandcover",
      arguments = list()
    ),
    nelson_et_al = list(
      type = "raster",
      source = "Travel Time to Cities and Ports 2015",
      downloader = ".get_nelson_et_al",
      arguments = list(
        range_traveltime = "20k_50k"
      )
    ),
    nasa_grace = list(
      type = "raster",
      source = "NASA GRACE",
      downloader = ".get_nasa_grace",
      arguments = list()
    ),
    worldclim_min_temperature = list(
      type = "raster",
      source = "WorldClim",
      downloader = ".get_worldclim_min_temperature",
      arguments = list()
    ),
    worldclim_max_temperature = list(
      type = "raster",
      source = "WorldClim",
      downloader = ".get_worldclim_max_temperature",
      arguments = list()
    ),
    worldclim_precipitation = list(
      type = "raster",
      source = "WorldClim",
      downloader = ".get_worldclim_precipitation",
      arguments = list()
    ),
    chirps = list(
      type = "raster",
      source = "",
      downloader = ".get_chirps",
      arguments = list()
    ),
    teow = list(
      type = "vector",
      source = "WWF",
      downloader = ".get_teow",
      arguments = list()
    ),
    gmw = list(
      type = "vector",
      source = "Global Mangrove Watch",
      downloader = ".get_gmw",
      arguments = list()
    ),
    nasa_srtm = list(
      type = "raster",
      source = "SRTM",
      downloader = ".get_nasa_srtm",
      arguments = list()
    ),
    soilgrids = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_soilgrids",
      arguments = list(
        layers = "clay",
        depths = "0-5cm",
        stats = "mean"
      )
    ),
    nasa_firms = list(
      type = "vector",
      source = "NASA FIRMS",
      downloader = ".get_nasa_firms",
      arguments = list(
        instrument = "VIIRS"
      )
    )
  )

  # determine what to return
  if (is.null(resources)) {
    return(all_resources[order(names(all_resources))])
  } else {
    .check_requested_resources(resources)
    all_resources[resources]
  }
}
