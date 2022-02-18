.get_ESALandCover <- function(bbox,
                              year = 2019,
                              rundir = tempdir()) {

  available_years = c(2015:2019)
  if(!year %in% available_years) {
    stop(sprintf("Year %s is not an available land cover layer. Please choose one of: %s", year, paste(available_years, collapse = ", ")))
  }

  # make the GFW grid and construct urls for intersecting tiles
  grid_esa = .makeESAGrid()
  tile_ids = st_intersects(bbox, grid_esa)[[1]]
  if(length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the Land Cover grid.")
  urls = sapply(tile_ids, function(n) .getEsaURL(grid_esa[n, ], year))

  for (url in urls){
    download.file(url, file.path(rundir, basename(url)))
    #command = sprintf("gdal_translate %s %s -of COG -co COMPRESS=LZW", url, file.path(rundir, basename(url)))
    #system(command)
  }
  # return all paths to the downloaded files
  list.files(rundir, full.names = T)

}


.makeESAGrid <- function(xmin=-180, xmax=180, dx=20, ymin=-60, ymax=80, dy=20,
                         proj=NULL) {
  if (is.null(proj)) proj = st_crs(4326)
  ncells = c((xmax - xmin) / dx,
             (ymax - ymin) / dy)

  bbox = st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin))
  st_as_sf(st_make_grid(bbox, cellsize = 20, n = ncells, crs = "EPSG:4326", what = "polygons"))

}



.getEsaURL <- function(tile, year) {
  min_x = st_bbox(tile)[1]
  max_y = st_bbox(tile)[4]

  # prepare tile names
  if (min_x < 0) {
    min_x = paste0('W', sprintf('%03i', abs(min_x)))
  } else {
    min_x = paste0('E', sprintf('%03i', min_x))
  }
  if (max_y < 0) {
    max_y = paste0('S', sprintf('%02i', abs(max_y)))
  } else {
    max_y = paste0('N', sprintf('%02i', max_y))
  }

  grid = paste0(min_x, max_y)

  if (year == 2015) {
    url = paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-base_Discrete-Classification-map_EPSG-4326.tif")
  } else if (year == 2019) {
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-nrt_Discrete-Classification-map_EPSG-4326.tif")
  } else {
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-conso_Discrete-Classification-map_EPSG-4326.tif")
  }
  url

}

