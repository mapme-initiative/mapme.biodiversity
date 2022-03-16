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
      )
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
      )
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
      )
    ),
    elevation = list(
      name = ".calc_dem",
      inputs = list(srtmelevation = "raster"),
      arguments = list(
        stats = "mean",
        engine = "zonal"
      )
    ),
    tri <- list(
      name = ".calc_tri",
      inputs = list(srtmelevation = "raster"),
      arguments = list(
        stats = "mean",
        engine = "zonal"
      )
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
