.get_droughtInd <- function(x,
                            rundir = tempdir(),
                            verbose = TRUE) {

  target_years = attributes(x)$years
  urls = unlist(sapply(target_years, function(year) .getDroughtIndURL(year)))

  # start download in a temporal directory within tmpdir
  if(verbose) pb = progress_bar$new(total = length(urls))
  for (url in urls){

    tryCatch(
      {
        if(verbose) pb$tick(0)
        download.file(url, file.path(rundir, basename(url)), quiet = TRUE)
        if(verbose) pb$tick()

      }, error = function(e) {
        message('reading URLs!')
      }
    )
  }
  # return paths to the raster
  list.files(rundir, full.names = T)
}




.getDroughtIndURL <- function(target_year) {

  available_years = c(2003:2022)
  dates = seq.Date(as.Date("2003/02/03"), as.Date("2022/02/14"), by = "week")
  available_dates = as.integer(format(dates, "%Y%m%d"))

  if (target_year %in% available_years) {

    target_dates = subset(available_dates, substr(available_dates, 1, 4) == target_year)
    url = paste0("https://nasagrace.unl.edu/globaldata/",target_dates,"/gws_perc_025deg_GL_",target_dates,".tif")
    url
  } else {
    warning(sprintf("Drought indicator not available for target year %s", target_year))
    NULL
  }
}

