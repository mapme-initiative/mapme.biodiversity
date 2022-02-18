.get_droughtInd <- function(bbox,
                            timeframe = 20220131,
                            rundir = tempdir()) {

  # get url
  url = paste0("https://nasagrace.unl.edu/globaldata/",timeframe,"/gws_perc_025deg_GL_",timeframe,".tif")
  # download the file
  download.file(url, file.path(rundir, basename(url)))
  # return paths to the raster
  list.files(rundir, full.names = T)

}

