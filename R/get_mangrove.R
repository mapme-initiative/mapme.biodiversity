.get_mangrove <- function(bbox,
                          year = 2016,
                          rundir = tempdir()) {

  available_years = c(1996, 2007:2010, 2015, 2016)
  if(!year %in% available_years) {
    stop(sprintf("Year %s is not an available mangrove extent layer. Please choose one of: %s", year, paste(available_years, collapse = ", ")))
  }

  url = paste0("https://wcmc.io/GMW_", year)

  if (year == 2007) {

    # download the file
    download.file(url, file.path(rundir, basename(paste0("global-mangrove-watch-2007.zip"))))
    # unzip
    unzip(zipfile = file.path(rundir, basename(paste0("global-mangrove-watch-2007.zip"))),
          exdir = rundir)
    # load shapefile
    shp = read_sf(paste0(rundir, "/GMW_2007_v2.0.shp"))
    # write as geopackage
    write_sf(shp, file.path(rundir, basename("global-mangrove-watch-2007.gpkg")))
    # delete zip file
    all_files = list.files(rundir, full.names = T)
    unlink(grep(paste0("global-mangrove-watch-2007.gpkg"), all_files, value = T, invert = T), recursive = T, force = T)
    # return paths to the raster
    list.files(rundir, full.names = T)

  } else {

    # download the file
    download.file(url, file.path(rundir, basename(paste0("global-mangrove-watch-",year,".zip"))))
    # unzip
    unzip(zipfile = file.path(rundir, basename(paste0("global-mangrove-watch-",year,".zip"))),
          exdir = rundir)
    # load shapefile
    shp = read_sf(paste0(rundir, "/GMW_001_GlobalMangroveWatch_",year,"/01_Data/GMW_",year,"_v2.shp"))
    # write as geopackage
    write_sf(shp, file.path(rundir, basename(paste0("global-mangrove-watch-",year,".gpkg"))))
    # delete zip file
    all_files = list.files(rundir, full.names = T)
    unlink(grep(paste0("global-mangrove-watch-",year,".gpkg"), all_files, value = T, invert = T), recursive = T, force = T)
    # return paths to the raster
    list.files(rundir, full.names = T)
  }

}

