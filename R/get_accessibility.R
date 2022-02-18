.get_accessibility <- function(bbox,
                               range = "20k_50k",
                               rundir = tempdir()) {

  available_layers = c("5k_10k", "10k_20k", "20k_50k", "50k_100k", "100k_200k", "200k_500k", "500k_1mio",
                       "1mio_5mio", "50k_50mio", "5k_110mio", "20k_110mio")
  if(!range %in% available_layers) {
    stop(sprintf("Layer %s is not an available accessibility layer. Please choose one of: %s", range, paste(available_layers, collapse = ", ")))
  }
  # get url
  url = .getaccessibilityURL(range)
  # download the file
  download.file(url, file.path(rundir, basename(paste0("accessibility-",range,".tif"))))
  # return paths to the raster
  list.files(rundir, full.names = T)

}


.getaccessibilityURL <- function(range){


  df.index = data.frame(range = c("5k_10k", "10k_20k", "20k_50k", "50k_100k", "100k_200k", "200k_500k", "500k_1mio",
                                  "1mio_5mio", "50k_50mio", "5k_110mio", "20k_110mio"),
                        index = c(14189840, 14189837, 14189831, 14189825, 14189819, 14189816, 14189810,
                                  14189807, 14189849, 14189852, 14189843))

  index = df.index$index[df.index$range==range]
  url = paste0("https://ndownloader.figshare.com/files/", index)
  url

}
