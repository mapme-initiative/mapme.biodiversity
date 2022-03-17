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
#' head(available_indicators(), 3)

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

