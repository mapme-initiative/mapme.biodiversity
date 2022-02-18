.get_popCount <- function(bbox,
                          year = 2020,
                          rundir = tempdir()) {

  available_layers = c(2000:2020)
  if(!year %in% available_layers) {
    stop(sprintf("Layer %s is not an available population count layer. Please choose one of: %s", year, paste(available_layers, collapse = ", ")))
  }

  # get url
  url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/", year, "/0_Mosaicked/ppp_", year, "_1km_Aggregated.tif")
  # download the file
  download.file(url, file.path(rundir, basename(url)))
  # return paths to the raster
  list.files(rundir, full.names = T)

}

