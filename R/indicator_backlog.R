available_indicators <- function(indicator = NULL) {
  all_indicators <- list(
    treecover = list(
      name = ".calc_cover",
      inputs = list(
        treecover =
          "raster",
        lossyear = "raster"
      ),
      arguments = list(
        minSize = 10,
        minCover = 30
      )
    ),
    emissions = list(
      name = ".calc_emissions",
      inputs = list(
        treecover = "raster",
        lossyear = "raster",
        greenhouse = "raster"
      ),
      arguments = list(
        minSize = 10,
        minCover = 30
      )
    ),
    elevation = list(
      name = ".calc_dem",
      inputs = list(srtmelevation = "raster"),
      arguments = list(
        stats = "mean",
        engine = 'zonal'
      )
    ),
    tri <- list(
      name = ".calc_tri",
      inputs = list(srtmelevation = "raster"),
      arguments = list(
        stats = "mean",
        engine = 'zonal'
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
