.get_climaticVariables <- function(bbox,
                                   layer = "prec",
                                   year = 2018,
                                   month = "01",
                                   rundir = tempdir()) {
  # check available layers
  available_layers = c("tmin", "tmax", "prec")
  if(!layer %in% available_layers) {
    stop(sprintf("Layer %s is not an available climatic layer. Please choose one of: %s", layer, paste(available_layers, collapse = ", ")))
  }

  # check years
  available_years = c(2000:2018)
  if(!year %in% available_years) {
    stop(sprintf("Year %s is not an available climatic layer. Please choose one of: %s", year, paste(available_years, collapse = ", ")))
  }

  # check years
  available_months = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  if(!month %in% available_months) {
    stop(sprintf("Month %s is not an available climatic layer. Please choose (as string) one of: %s", month, paste(available_months, collapse = ", ")))
  }

  # get url
  url = .getClimateURL(layer, year)
  # download the file
  download.file(url, file.path(rundir, basename(paste0(layer, "_", year,".zip"))))
  # unzip
  unzip(zipfile = file.path(rundir, basename(paste0(layer, "_", year,".zip"))),
        exdir = rundir)
  # remove all except desired layers
  all_files = list.files(rundir, full.names = T)
  unlink(grep(paste0("wc2.1_2.5m_",layer,"_",year,"-",month,".tif"), all_files, value = T, invert = T), recursive = T, force = T)
  # return paths to the raster
  list.files(rundir, full.names = T)

}


.getClimateURL <- function(layer, year){

  if (layer == "tmin") {
    if (year %in% 2000:2009) {
      url = paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_2000-2009.zip")
    } else {
      url = paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_2010-2018.zip")}
  }

  if (layer == "tmax") {
    if (year %in% 2000:2009) {
      url = paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_2000-2009.zip")
    } else {
      url = paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_2010-2018.zip")}
  }

  if (layer == "prec") {
    if (year %in% 2000:2009) {
      url = paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_2000-2009.zip")
    } else {
      url = paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_2010-2018.zip")}
  }

  url
}
