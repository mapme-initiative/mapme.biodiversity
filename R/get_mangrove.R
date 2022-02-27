.get_mangrove <- function(x,
                          rundir = tempdir(),
                          verbose = TRUE) {

  target_years = attributes(x)$years
  urls = unlist(sapply(target_years, function(year) .getMangroveURL(year)))

  # start download in a temporal directory within tmpdir
  if(verbose) pb = progress_bar$new(total = length(urls))
  for (url in urls) {

    tryCatch(
      {
        if(verbose) pb$tick(0)
        year = gsub(".*?([0-9]+).*", "\\1", url)
        download.file(url, file.path(rundir, basename(paste0("gmw-extent_",year,".zip"))), quiet = TRUE)
        if(verbose) pb$tick()

      }, error = function(e) {
        message('reading URLs!')
      }
    )
  }

  # unzip and convert shp to gpkg
  all_zips = list.files(rundir, full.names = T)
  sapply(all_zips, function(zip) .unzipMangrove(zip, rundir))

  # delete unneccessary files
  d_files = list.files(rundir, full.names = T)
  unlink(grep("*gpkg", d_files, value = T, invert = T), recursive = T, force = T)
  # return paths to the gpkg
  list.files(rundir, full.names = T)

}



.unzipMangrove <- function(zip_files, rundir) {

  bn = basename(zip_files)
  if (bn == "gmw-extent_2007.zip") {

    unzip(zipfile = file.path(rundir, basename(paste0("gmw-extent_2007.zip"))),
          exdir = rundir)
    shp = read_sf(paste0(rundir, "/GMW_2007_v2.0.shp"))
    st_write(shp, file.path(rundir, basename("gmw-extent_2007.gpkg")))
    d_files = list.files(rundir, full.names = T)
    unlink(grep("gmw-extent*", d_files, value = T, invert = T), recursive = T, force = T)

  }  else {
    year = gsub(".*?([0-9]+).*", "\\1", bn)
    unzip(zipfile = file.path(rundir, basename(paste0("gmw-extent_",year,".zip"))),
          exdir = rundir)
    shp = read_sf(paste0(rundir, "/GMW_001_GlobalMangroveWatch_",year,"/01_Data/GMW_",year,"_v2.shp"))
    st_write(shp, file.path(rundir, basename(paste0("gmw-extent_",year,".gpkg"))))
    d_files = list.files(rundir, full.names = T)
    unlink(grep("gmw-extent*", d_files, value = T, invert = T), recursive = T, force = T)
  }
}




.getMangroveURL <- function(target_year) {

  available_years = c(1996, 2007:2010, 2015, 2016)
  if (target_year %in% available_years) {

    url = paste0("https://wcmc.io/GMW_", target_year)
    url
  } else {
    warning(sprintf("Mangove extent not available for target year %s", target_year))
    NULL
  }
}

