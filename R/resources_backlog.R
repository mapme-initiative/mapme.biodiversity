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
#'
available_resources <- function(resources = NULL){
  all_resources = list(
    "treecover" = list(type = "raster",
                       source = "Global Forest Watch  (GFW)",
                       downloader = ".get_treecover",
                       arguments = list(
                         "vers_treecover" = "GFC-2019-v1.7")
    ),
    "lossyear" = list(type = "raster",
                      "source" = "Global Forest Watch  (GFW)",
                      "downloader" = ".get_lossyear",
                      "arguments" = list(
                        "vers_lossyear" = "GFC-2019-v1.7"
                      )
    ),
    "greenhouse" = list("type" = "raster",
                        "source" = "Global Forest Watch (GFW)",
                        "downloader" = ".get_greenhouse",
                        "arguments" = list(
                          "vers_greenhouse" = "v20211022",
                          "api_key_gfw" = NA
                        )
    ),
    "populationcount" = list("type" = "raster",
                             "source" = "WorldPop",
                             "downloader" = ".get_popCount",
                             "arguments" = list(

                             )
    ),
    "esalandcover" = list("type" = "raster",
                          "source" = "Copernicus (ESA)",
                          "downloader" = ".get_ESALandCover",
                             "arguments" = list(

                             )
    ),
    "accessibility" = list("type" = "raster",
                           "source" = "Travel Time to Cities and Ports 2015 (Weiß et al. (2018))",
                           "downloader" = ".get_accessibility",
                           "arguments" = list(
                             "range" = "20k_50k"
                           )
    ),
    "droughtindicators" = list("type" = "raster",
                               "source" = "NASA GRACE",
                               "downloader" = ".get_droughtInd",
                               "arguments" = list(

                               )
    ),
    "mintemperature" = list("type" = "raster",
                            "source" = "WorldClim",
                            "downloader" = ".get_minTemperature",
                               "arguments" = list(

                               )
    ),
    "maxtemperature" = list("type" = "raster",
                            "source" = "WorldClim",
                            "downloader" = ".get_maxTemperature",
                            "arguments" = list(

                            )
    ),
    "precipitation" = list("type" = "raster",
                           "source" = "WorldClim",
                           "downloader" = ".get_precipitation",
                           "arguments" = list(

                            )
    ),
    "ecoregions" = list("type" = "vector",
                        "source" = "WWF",
                        "downloader" = ".get_ecoregions",
                        "arguments" = list(

                       )
    ),
    "mangroveextent" = list("type" = "vector",
                            "source" = "Global Mangrove Watch",
                            "downloader" = ".get_mangrove",
                            "arguments" = list(

                            )
    ),
    "srtmelevation" = list("type" = "raster",
                           "source" = "SRTM",
                           "downloader" = ".get_SRTMdem",
                           "arguments" = list(

                            )
    )
  )

  # determine what to return
  if(is.null(resources)) {
    return(all_resources)
  } else {
    .check_requested_resources(resources)
    all_resources[resources]
  }

}
