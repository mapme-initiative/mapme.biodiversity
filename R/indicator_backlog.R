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
    treecover = list(
      name = ".calc_treecover",
      inputs = list(
        treecover2000 = "raster",
        lossyear = "raster"
      ),
      arguments = list(
        min_size = 10,
        min_cover = 30
      ),
      processing_mode = "asset"
    ),
    emissions = list(
      name = ".calc_emissions",
      inputs = list(
        treecover2000 = "raster",
        lossyear = "raster",
        greenhouse = "raster"
      ),
      arguments = list(
        min_size = 10,
        min_cover = 30
      ),
      processing_mode = "asset"
    ),
    treeloss = list(
      name = ".calc_treeloss",
      inputs = list(
        treecover2000 = "raster",
        lossyear = "raster",
        greenhouse = "raster"
      ),
      arguments = list(
        min_size = 10,
        min_cover = 30
      ),
      processing_mode = "asset"
    ),
    elevation = list(
      name = ".calc_dem",
      inputs = list(srtmdem = "raster"),
      arguments = list(
        stats_elevation = "mean",
        engine = "zonal"
      ),
      processing_mode = "asset"
    ),
    tri = list(
      name = ".calc_tri",
      inputs = list(srtmdem = "raster"),
      arguments = list(
        stats_tri = "mean",
        engine = "zonal"
      ),
      processing_mode = "asset"
    ),
    precipitation = list(
      name = ".calc_precipitation",
      inputs = list(chirps = "raster"),
      arguments = list(
        scales_spi = NULL,
        engine = "extract"
      ),
      processing_mode = "portfolio"
    ),
    accessibility = list(
      name = ".calc_accessibility",
      inputs = list(traveltime = "raster"),
      arguments = list(
        stats_accessibility = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    drought_indicator = list(
      name = ".calc_drought_indicator",
      inputs = list(nasagrace = "raster"),
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
    popcount = list(
      name = ".calc_worldpop",
      inputs = list(worldpop = "raster"),
      arguments = list(
        stats_worldpop = "sum",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    landcover = list(
      name = ".calc_esalandcover",
      inputs = list(esalandcover = "raster"),
      arguments = list(
      ),
      processing_mode = "asset"
    ),
    worldclim_tmin = list(
      name = ".calc_worldclim_mintemperature",
      inputs = list(mintemperature = "raster"),
      arguments = list(
        stats_mintemperature = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    worldclim_tmax = list(
      name = ".calc_worldclim_maxtemperature",
      inputs = list(maxtemperature = "raster"),
      arguments = list(
        stats_maxtemperature = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    ),
    worldclim_prec = list(
      name = ".calc_worldclim_precipitation",
      inputs = list(precipitation = "raster"),
      arguments = list(
        stats_precipitation = "mean",
        engine = "extract"
      ),
      processing_mode = "asset"
    )
  )

  # determine what to return
  if (is.null(indicator)) {
    return(all_indicators)
  } else {
    .check_requested_indicator(indicator)
    all_indicators[indicator]
  }
}
