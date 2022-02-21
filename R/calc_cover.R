#' Calculate tree cover per year based on GFW data sets
#'
#' Considering the 2000 GFW forest cover density layer users can specify
#' a cover threshold above which a pixel is considered to be covered by forest.
#' Additionally, users can specify a minimum size of a comprehensive patch of
#' forest pixels. Patches below this threshold will not be considered as forest area.
#'
#' @param shp A single polygon for which to calculate the tree cover statistic
#' @param cover The treecover 2000 resource from GFW
#' @param loss The lossyear resource from GFW
#' @param minSize The minimum size of a patch of pixels to be considered as forest area (in pixels)
#' @param minCover The minimum threshold of stand density for a pixel to be considered forest in the year 2000.
#' @param years The years for which to apply the analysis.
#'
#' @return A tibble
#' @importFrom stringr str_sub
#' @export
#'
.calc_cover <- function(shp, treecover, lossyear, minSize = 10, minCover = 35, years = 2010:2018){

  minCover_msg = "Argument 'minCover' for indicator 'cover' must be a numeric value between 0 and 100."
  if(is.numeric(minCover)){
    minCover = as.integer(round(minCover))
  } else {
    stop(minCover_msg)
  }
  if(minCover < 0 || minCover > 100){
    stop(minCover_msg)
  }

  minSize_msg = "Argument 'minSize' for indicator 'cover' must be a numeric value greater 0."
  if(is.numeric(minSize)){
    minSize = as.integer(round(minSize))
  } else {
    stop(minSize_msg)
  }
  if(minSize <= 0) stop(minSize_msg)

  treecover = classify(treecover, rcl = matrix(c(0, minCover, 0, minCover, 100, 1), ncol = 3, byrow = TRUE))
  patched = patches(treecover, directions = 8, zeroAsNA = TRUE)
  patched_df = as.data.frame(freq(patched))
  ids_to_exclude = which(patched_df$count < minSize)
  if(length(ids_to_exclude) > 0 ){
    ids = patched_df[ids_to_exclude, ]$value
    patched[patched %in% ids] = NA
  }
  patched[!is.na(patched)] = 1

  cover_layers = lapply(years, function(y){
    y = y - 2000
    loss_before = lossyear < y
    cover_current = treecover
    cover_current[loss_before] = 0
    cover_current
  })

  cover_layers = do.call(c, cover_layers)
  names(cover_layers) = paste("cover_", years, sep = "")

  area_raster =  cover_layers * cellSize(treecover, unit = "ha")

  stats = terra::extract(area_raster, vect(shp$geom), "sum")
  # stats %<>%
  #   pivot_longer(2:ncol(stats), names_to = "year", values_to = "cover") %>%
  #   mutate(year = as.numeric(str_sub(year, -4, -1)))
  stats$ID = NULL
  stats
}
