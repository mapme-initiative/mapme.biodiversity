.get_ecoregions <- function(bbox,
                            rundir = tempdir()) {

  # get url
  url = "https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/6kcchn7e3u_official_teow.zip"
  # download the file
  download.file(url, file.path(rundir, basename(paste0("TEOW_global.zip"))))
  # unzip
  unzip(zipfile = file.path(rundir, basename(paste0("TEOW_global.zip"))),
        exdir = rundir)
  # load shp
  shp = read_sf(paste0(rundir, "/official/wwf_terr_ecos.shp"))
  # write as gpkg
  write_sf(shp,
           file.path(rundir, basename(paste0("teow-global.gpkg"))))
  # remove all except desired layers
  all_files = list.files(rundir, full.names = T)
  unlink(grep(paste0("teow-global.gpkg"), all_files, value = T, invert = T), recursive = T, force = T)
  # return paths to the raster
  list.files(rundir, full.names = T)

}

