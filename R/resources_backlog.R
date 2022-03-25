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
#' head(available_resources(), 3)
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
    bdod = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_bdod",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = "Bulk density of the fine earth fraction",
        mapped_units = "cg/cm3",
        conversion_factor = 100,
        conventional_units = "kg/dm3"
      )
    ),
    cec = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_cec",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = "Cation Exchange Capacity of the soil",
        mapped_units = "mmol(c)/kg",
        conversion_factor = 10,
        conventional_units = "cmol(c)/kg"
      )
    ),
    cfvo = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_cfvo",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = "Volumetric fraction of coarse fragments (> 2 mm)",
        mapped_units = "cm3/dm3 (volPerc)",
        conversion_factor = 10,
        conventional_units = "cm3/100cm3 (volperc)"
      )
    ),
    clay = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_clay",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = paste("Proportion of clay particles (< 0.002 mm)",
          "in the fine earth fraction",
          sep = ""
        ),
        mapped_units = "g/kg",
        conversion_factor = 10,
        conventional_units = "g/100g (Perc)"
      )
    ),
    nitrogen = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_nitrogen",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = "Total nitrogen (N)",
        mapped_units = "cg/kg",
        conversion_factor = 100,
        conventional_units = "g/kg"
      )
    ),
    phh2o = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_phh2o",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = "Soil pH",
        mapped_units = "pHx10",
        conversion_factor = 100,
        conventional_units = "pH"
      )
    ),
    sand = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_sand",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = paste("Proportion of sand particles (> 0.05 mm) ",
          "in the fine earth fraction",
          sep = ""
        ),
        mapped_units = "g/kg",
        conversion_factor = 10,
        conventional_units = "g/100g (perc)"
      )
    ),
    silt = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_silt",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = paste("Proportion of silt particles (>= 0.002 mm and ",
          "<= 0.05 mm) in the fine earth fraction",
          sep = ""
        ),
        mapped_units = "g/kg",
        conversion_factor = 10,
        conventional_units = "g/100g (perc)"
      )
    ),
    soc = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_silt",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = "Soil organic carbon content in the fine earth fraction",
        mapped_units = "dg/kg",
        conversion_factor = 10,
        conventional_units = "g/kg"
      )
    ),
    ocd = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_cec",
      arguments = list(
        depth = "0-5cm",
        stat = "mean"
      ),
      description = list(
        long_name = "Organic carbon density",
        mapped_units = "kg/m3",
        conversion_factor = 10,
        conventional_units = "kg/m3"
      )
    ),
    ocs = list(
      type = "raster",
      source = "https://www.isric.org/explore/soilgrids",
      downloader = ".get_ocs",
      arguments = list(
        depth = "0-30cm",
        stat = "mean"
      ),
      description = list(
        long_name = "Organic carbon stocks",
        mapped_units = "t/ha",
        conversion_factor = 10,
        conventional_units = "kg/m2"
      )
    )
  )

  # determine what to return
  if (is.null(resources)) {
    return(all_resources)
  } else {
    .check_requested_resources(resources)
    all_resources[resources]
  }
}
