.get_accessibility <- function(x,
                               range_accessibility = "20k_50k",
                               rundir = tempdir(),
                               verbose = TRUE) {

  # get url
  url = .getaccessibilityURL(range_accessibility)
  # start download in a temporal directory within tmpdir
  if(verbose) pb = progress_bar$new(total = length(url))
  if(verbose) pb$tick(0)
  download.file(url, file.path(rundir, basename(paste0("accessibility-",range,".tif"))), quiet = TRUE)
  if(verbose) pb$tick()

  # return paths to the raster
  list.files(rundir, full.names = T)

}


.getaccessibilityURL <- function(range){


  df.index = data.frame(range = c("5k_10k", "10k_20k", "20k_50k", "50k_100k", "100k_200k", "200k_500k", "500k_1mio",
                                  "1mio_5mio", "50k_50mio", "5k_110mio", "20k_110mio"),
                        index = c(14189840, 14189837, 14189831, 14189825, 14189819, 14189816, 14189810,
                                  14189807, 14189849, 14189852, 14189843))

  if (range %in% df.index$range) {
    index = df.index$index[df.index$range==range]
    url = paste0("https://ndownloader.figshare.com/files/", index)
    url
  } else {
    warning(sprintf("Accessibility layer not available for range %s", range))
    NULL
  }
}

