available_indicators <- function(indicator = NULL){
  all_indicators = list(
    "cover" = list(
      name = ".calc_cover",
      inputs = list("treecover" = "raster", "lossyear" = "raster"),
      arguments = list("minSize" = 10,
                  "minCover" = 30)
    )
  )

  # determine what to return
  if(is.null(indicator)) {
    return(all_indicators)
  } else {
    .check_requested_indicator(indicator)
    all_indicators[indicator]
  }
}
