#' Calculate tree cover per year based on GFW data sets
#'
#' Considering the 2000 GFW forest cover density layer users can specify
#' a cover threshold above which a pixel is considered to be covered by forest.
#' Additionally, users can specify a minimum size of a comprehensive patch of
#' forest pixels. Patches below this threshold will not be considered as forest area.
#'
#' @param shp A single polygon for which to calculate the tree cover statistic
#' @param treecover The treecover 2000 resource from GFW
#' @param lossyear The lossyear resource from GFW
#' @param greenhouse The greenhouse emission layer from GFW
#' @param minSize The minimum size of a forest patch in ha.
#' @param minCover The minimum threshold of stand density for a pixel to be considered forest in the year 2000.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall be written to disk
#' @return A tibble
#' @importFrom stringr str_sub
#' @export
#'
.calc_emissions <- function(shp,
                        treecover,
                        lossyear,
                        greenhouse,
                        minSize = 10,
                        minCover = 35,
                        rundir = tempdir(),
                        verbose = TRUE,
                        todisk = FALSE){

  # initial argument checks
  # retrieve years from portfolio
  years = attributes(shp)$years
  # handling of return value if resources are missing, e.g. no overlap
  if(any(missing(treecover), missing(lossyear))){
    return(tibble(years = years, emissions = rep(NA, length(years))))
  }
  # check if treecover only contains 0s, e.g. on the ocean
  if(all(length(unique(unlist(minmax(treecover)))) == 1, unique(unlist(minmax(treecover))) == 0)){
    return(tibble(years = years, emissions = rep(0, length(years))))
  }

  # check additional arguments
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

  if(any(years < 2000)){
    warning("Cannot calculate tree cover statistics for years smaller than 2000.")
    years = years[years >= 2000]
  }

  #------------------------------------------------------------------------------
  # start calculation if everything is set up correctly
  # retrieve an area raster
  arearaster = cellSize(treecover, unit = "ha",
                        filename = ifelse(todisk, file.path(rundir, "arearaster.tif"), ""),
                        datatype = "FLT4S",
                        overwrite = TRUE)
  # binarize the treecover layer based on minCover argument
  binary_treecover = classify(treecover,
                              rcl = matrix(c(0, minCover, 0, minCover, 100, 1), ncol = 3, byrow = TRUE),
                              filename = ifelse(todisk, file.path(rundir, "binary_treecover.tif"), ""),
                              datatype = "INT1U",
                              overwrite = TRUE)
  # retrieve patches of comprehensive forest areas
  patched = patches(binary_treecover, directions = 8, zeroAsNA = TRUE,
                    filename = ifelse(todisk, file.path(rundir, "patched.tif"), ""),
                    datatype = "INT4U",
                    overwrite = TRUE)
  # get the sizes of the patches
  patchsizes = zonal(arearaster, patched, sum, as.raster = TRUE,
                     filename = ifelse(todisk, file.path(rundir, "patchsizes.tif"), ""),
                     datatype = "FLT4S",
                     overwrite = TRUE)
  # remove patches smaller than threshold
  binary_treecover = ifel(patchsizes < minSize, 0, binary_treecover,
                          filename = ifelse(todisk, file.path(rundir, "binary_treecover.tif"), ""),
                          datatype = "INT1U",
                          overwrite = TRUE)
  # set no loss occurrences to NA
  lossyear = ifel(lossyear == 0, NA, lossyear,
                  filename = ifelse(todisk, file.path(rundir, "lossyear.tif"), ""),
                  datatype = "INT1U",
                  overwrite = TRUE)
  # rasterize the polygon
  polyraster = rasterize(vect(shp), binary_treecover, field = 1, touches = TRUE,
                         filename = ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
                         datatype = "INT1U",
                         overwrite = TRUE)
  # get forest cover statistics for each year
  yearly_emission_values = lapply(years, function(y){
    y = y - 2000
    current_losslayer = ifel(lossyear == y , 1, NA,
                             filename = ifelse(todisk, file.path(rundir, "current_treelosses.tif"), ""),
                             datatype = "INT1U",
                             overwrite = TRUE)
    current_greenhouse = mask(greenhouse, current_losslayer,
                              filename = ifelse(todisk, file.path(rundir, "current_co2_emissions.tif"), ""),
                              datatype = "FLT4S",
                              overwrite = TRUE)
    # terra engine
    emissions_sum = zonal(current_greenhouse, polyraster,  sum, na.rm = TRUE)[2]
    as.numeric(emissions_sum)
  })

  # memory clean up
  rm(arearaster, binary_treecover, patchsizes, patched, polyraster); gc()
  # return a data-frame
  tibble(years = years, emissions = as.vector(yearly_emission_values))
}
