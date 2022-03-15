#' Calculate elevation based on SRTM rasters
#'

.calc_SRTMElevation <- function(shp,
                                srtmelevation,
                                engine = "zonal",
                                stats = "mean",
                                rundir = tempdir(),
                                verbose = TRUE,
                                todisk = FALSE,
                                ...) {

  if (ncell(srtmelevation) > 1024 * 1024) todisk <- TRUE
  if (engine == "extract") {
    tib.zstats <- .comp_eleExtract(
      elevation = srtmelevation,
      shp = shp,
      stats = stats
    )
    return(tib.zstats)

  } else {
    tib.zstats <- .comp_eleZonal(
      elevation = srtmelevation,
      shp = shp,
      stats = stats,
      todisk = todisk,
      rundir = rundir
    )
    return(tib.zstats)
  }
}


#' function to compute single raster with terra extract
#'

.comp_eleExtract <- function(elevation = NULL,
                             shp = NULL,
                             stats = "mean",
                             ...) {
  shp.v <- vect(shp)
  zstats <- terra::extract(elevation,
    shp.v,
    fun = stats,
    na.rm = T
  )
  tibble.zstats <- tibble(elev = zstats[, 2])
  names(tibble.zstats)[names(tibble.zstats) == "elev"] <-
    paste0("elevation_", stats)
  return(tibble.zstats)
}


#' function to compute single raster with zonal
#'

.comp_eleZonal <- function(elevation = NULL,
                           shp = NULL,
                           stats = "mean",
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
  zstats <- terra::zonal(rast_mask,
    p_raster,
    fun = stats,
    na.rm = T
  )
  tibble.zstats <- tibble(elev = zstats[, 2])
  names(tibble.zstats)[names(tibble.zstats) == "elev"] <-
    paste0("elevation_", stats)
  return(tibble.zstats)
}

