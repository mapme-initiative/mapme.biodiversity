#' Calculate Terrain Ruggedness Index (TRI) based on SRTM rasters
#'

.calc_TRI <- function(shp,
                      srtmelevation,
                      engine = "zonal",
                      stats = c('mean', 'median', 'sd'),
                      rundir = tempdir(),
                      verbose = TRUE,
                      todisk = FALSE,
                      ...) {

  if (ncell(srtmelevation) > 1024 * 1024) todisk <- TRUE
  if (engine == "extract") {
    tib.zstats <- .comp_triExtract(
      elevation = srtmelevation,
      shp = shp,
      stats = stats,
      rundir = rundir,
      todisk = todisk
    )
    return(tib.zstats)

  } else {
    tib.zstats <- .comp_triZonal(
      elevation = srtmelevation,
      shp = shp,
      stats = stats,
      todisk = todisk,
      rundir = rundir
    )
    return(tib.zstats)
  }
}


#' function to compute single raster with zonal
#'

.comp_triZonal <- function(elevation = NULL,
                           shp = NULL,
                           stats = c("mean", "median", "sd"),
                           todisk = todisk,
                           rundir = rundir,
                           ...) {
  shp.v <- vect(shp)
  rast_mask <- terra::mask(elevation,
    shp.v,
    filename =  ifelse(todisk, file.path(rundir, "elevation.tif"), ""),
    overwrite = TRUE
  )
  p_raster <- terra::rasterize(shp.v,
    rast_mask,
    field = 1:nrow(shp.v),
    filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    overwrite = TRUE
  )
  tri <- terra::terrain(rast_mask,
    v = "TRI",
    unit = "degrees",
    neighbors = 8,
    filename = ifelse(todisk, file.path(rundir, "terrain.tif"), ""),
    overwrite = TRUE
  )
  tri.mean <- terra::zonal(tri,
    p_raster,
    fun = "mean",
    na.rm = T
  )
  tri.median <- terra::zonal(tri,
    p_raster,
    fun = "median",
    na.rm = T
  )
  tri.sd <- terra::zonal(tri,
    p_raster,
    fun = "sd",
    na.rm = T
  )
  tib.zstats <- tibble(
    terrain_ruggedness_index_mean = tri.mean[, 2],
    terrain_ruggedness_index_median = tri.median[, 2],
    terrain_ruggedness_index_standard_deviation = tri.sd[, 2]
  )
  return(tib.zstats)
}


#' function to compute single raster with terra extract
#'

.comp_triExtract <- function(elevation = NULL,
                             shp = NULL,
                             stats = c("mean", "median", "sd"),
                             todisk = todisk,
                             rundir = rundir,
                             ...) {
  shp.v <- vect(shp)
  rast_mask <- terra::mask(elevation,
    shp.v,
    filename =  ifelse(todisk, file.path(rundir, "elevation.tif")),
    overwrite = TRUE
  )
  tri <- terra::terrain(rast_mask,
    v = "TRI",
    unit = "degrees",
    neighbors = 8,
    filename = ifelse(todisk, file.path(rundir, "terrain.tif"), ""),
    overwrite = TRUE
  )
  tri.mean <- terra::extract(tri,
    shp.v,
    fun = "mean",
    na.rm = T
  )
  tri.median <- terra::extract(tri,
    shp.v,
    fun = "median",
    na.rm = T
  )
  tri.sd <- terra::extract(tri,
    shp.v,
    fun = "sd",
    na.rm = T
  )
  tib.zstats <- tibble(
    terrain_ruggedness_index_mean = tri.mean[, 2],
    terrain_ruggedness_index_median = tri.median[, 2],
    terrain_ruggedness_index_sd = tri.sd[, 2]
  )
  return(tib.zstats)
}
