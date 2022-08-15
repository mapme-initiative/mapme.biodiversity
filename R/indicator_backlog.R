#' Backlog function for available indicators
#'
#' This function returns a list of either all available indicators and some
#' additional metadata or for one or more requested indicators. It can be used
#' by users of the package to inform themselves about the available data sets
#' and to learn about potentially additional arguments that should be specified
#' when requesting the indicator.
#'
#' @param indicator Defaults to NULL meaning that a list with all available
#'   indicators will be returned. If a character vector is specified only the
#'   information about the requested indicator will be returned.
#'
#' @return A list object.
#' @export
#' @keywords indicator
#' @examples
#' names(available_indicators())
available_indicators <- function(indicator = NULL) {
  all_indicators <- list(
    treecover_area = list(
      name = ".calc_treecover_area",
      inputs = list(
        gfw_treecover = "raster",
        gfw_lossyear = "raster"
      ),
      arguments = list(
        min_size = 10,
        min_cover = 30
      ),
      processing_mode = "asset"
    ),
    treecoverloss_emissions = list(
      name = ".calc_treecoverloss_emissions",
      inputs = list(
        gfw_treecover = "raster",
        gfw_lossyear = "raster",
        gfw_emissions = "raster"
      ),
      arguments = list(
        min_size = 10,
        min_cover = 30
      ),
      processing_mode = "asset"
    ),
    treecover_area_and_emissions = list(
      name = ".calc_treecover_area_and_emissions",
      inputs = list(
        gfw_treecover = "raster",
        gfw_lossyear = "raster",
        gfw_emissions = "raster"
      ),
      arguments = list(
        min_size = 10,
        min_cover = 30
      ),
      processing_mode = "asset"
    ),
    elevation = list(
      name = ".calc_elevation",
      inputs = list(nasa_srtm = "raster"),
      arguments = list(
        stats_elevation = "mean",
        engine = "zonal"
      ),
      processing_mode = "asset"
    ),
    tri = list(
      name = ".calc_tri",
      inputs = list(nasa_srtm = "raster"),
      arguments = list(
        stats_tri = "mean",
        engine = "zonal"
      ),
      processing_mode = "asset"
    ),
    precipitation_chirps = list(
      name = ".calc_precipitation_chirps",
      inputs = list(chirps = "raster"),
      arguments = list(
        scales_spi = 3,
        spi_prev_years = 8,
        engine = "extract"
      ),
      processing_mode = "portfolio"
    ),
    traveltime = list(
      name = ".calc_traveltime",
      inputs = list(nelson_et_al = "raster"),
      arguments = list(
        stats_accessibility = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    drought_indicator = list(
      name = ".calc_drought_indicator",
      inputs = list(nasa_grace = "raster"),
      arguments = list(
        stats_drought = "mean",
        engine = "extract"
      ),
      processing_mode = "portfolio"
    ),
    soilproperties = list(
      name = ".calc_soilproperties",
      inputs = list(soilgrids = "raster"),
      arguments = list(
        stats_soil = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    population_count = list(
      name = ".calc_population_count",
      inputs = list(worldpop = "raster"),
      arguments = list(
        stats_popcount = "sum",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    landcover = list(
      name = ".calc_landcover",
      inputs = list(esalandcover = "raster"),
      arguments = list(),
      processing_mode = "asset"
    ),
    temperature_min_wc = list(
      name = ".calc_temperature_min_wc",
      inputs = list(worldclim_min_temperature = "raster"),
      arguments = list(
        stats_worldclim = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    temperature_max_wc = list(
      name = ".calc_temperature_max_wc",
      inputs = list(worldclim_max_temperature = "raster"),
      arguments = list(
        stats_worldclim = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    precipitation_wc = list(
      name = ".calc_precipitation_wc",
      inputs = list(worldclim_precipitation = "raster"),
      arguments = list(
        stats_worldclim = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    mangroves_area = list(
      name = ".calc_mangroves_area",
      inputs = list(gmw = "vector"),
      arguments = list(),
      processing_mode = "asset"
    ),
    ecoregion = list(
      name = ".calc_ecoregion",
      inputs = list(teow = "vector"),
      arguments = list(),
      processing_mode = "asset"
    ),
    biome = list(
      name = ".calc_biome",
      inputs = list(teow = "vector"),
      arguments = list(),
      processing_mode = "asset"
    ),
    active_fire_properties = list(
      name = ".calc_active_fire_properties",
      inputs = list(nasa_firms = "vector"),
      arguments = list(),
      processing_mode = "asset"
    ),
    active_fire_counts = list(
      name = ".calc_active_fire_counts",
      inputs = list(nasa_firms = "vector"),
      arguments = list(),
      processing_mode = "asset"
    )
  )

  # determine what to return
  if (is.null(indicator)) {
    return(all_indicators[order(names(all_indicators))])
  } else {
    .check_requested_indicator(indicator)
    all_indicators[indicator]
  }
}
