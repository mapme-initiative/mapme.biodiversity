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
    treecover2000 = list(
      type = "raster",
      source = "https://data.globalforestwatch.org/documents/tree-cover-2000/explore",
      downloader = ".get_treecover",
      arguments = list(
        vers_treecover = "GFC-2020-v1.8"
      )
    ),
    lossyear = list(
      type = "raster",
      source = "https://data.globalforestwatch.org/documents/tree-cover-loss/explore",
      downloader = ".get_lossyear",
      arguments = list(
        vers_lossyear = "GFC-2020-v1.8"
      )
    ),
    greenhouse = list(
      "type" = "raster",
      source = "https://data.globalforestwatch.org/datasets/gfw::forest-greenhouse-gas-emissions/about",
      downloader = ".get_greenhouse",
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
    traveltime = list(
      type = "raster",
      source = "Travel Time to Cities and Ports 2015",
      downloader = ".get_traveltime",
      arguments = list(
        range_traveltime = "20k_50k"
      )
    ),
    nasagrace = list(
      type = "raster",
      source = "NASA GRACE",
      downloader = ".get_nasagrace",
      arguments = list()
    ),
    mintemperature = list(
      type = "raster",
      source = "WorldClim",
      downloader = ".get_min_temperature",
      arguments = list()
    ),
    maxtemperature = list(
      type = "raster",
      source = "WorldClim",
      downloader = ".get_max_temperature",
      arguments = list()
    ),
    precipitation = list(
      type = "raster",
      source = "WorldClim",
      downloader = ".get_precipitation",
      arguments = list()
    ),
    chirps = list(
      type = "raster",
      source = "",
      downloader = ".get_chirps",
      arguments = list()
    ),
    ecoregions = list(
      type = "vector",
      source = "WWF",
      downloader = ".get_ecoregions",
      arguments = list()
    ),
    mangrove = list(
      type = "vector",
      source = "Global Mangrove Watch",
      downloader = ".get_mangrove",
      arguments = list()
    ),
    srtmdem = list(
      type = "raster",
      source = "SRTM",
      downloader = ".get_srtmdem",
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
