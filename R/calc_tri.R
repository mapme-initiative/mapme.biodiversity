#' Calculate Terrain Ruggedness Index (TRI) based on SRTM rasters
#' @keywords internal

.calc_tri <- function(shp,
                      srtmelevation,
                      engine = "zonal",
                      stats = "mean",
                      rundir = tempdir(),
                      verbose = TRUE,
                      todisk = FALSE,
                      ...) {

  # check if input engines are correct
  available_engines <- c("zonal", "extract", "exactextract")
  if (!engine %in% available_engines) {
    stop(sprintf("Engine %s is not an available engine. Please choose one of: %s", engine, paste(available_engines, collapse = ", ")))
  }

  if (ncell(srtmelevation) > 1024 * 1024) todisk <- TRUE
  available_terra_stats <- c("mean", "median", "sd")
  available_exact_stats <- c("mean", "median", "stdev")

  if (engine == "extract") {

    # check if input stats are correct
    if (!stats %in% available_terra_stats) {
      stop(sprintf("Stat %s is not an available statistics. Please choose one of: %s", stats, paste(available_terra_stats, collapse = ", ")))
    }

    tibble_zstats <- .comp_tri_extract(
      elevation = srtmelevation,
      shp = shp,
      stats = stats,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  } else if (engine == "exactextract") {

    # check if input stats are correct
    if (!stats %in% available_exact_stats) {
      stop(sprintf("Stat %s is not an available statistics. Please choose one of: %s", stats, paste(available_exact_stats, collapse = ", ")))
    }

    tibble_zstats <- .comp_tri_exact_extractr(
      elevation = srtmelevation,
      shp = shp,
      stats = stats,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  } else {

    # check if input stats are correct
    if (!stats %in% available_terra_stats) {
      stop(sprintf("Stat %s is not an available statistics. Please choose one of: %s", stats, paste(available_terra_stats, collapse = ", ")))
    }

    tibble_zstats <- .comp_tri_zonal(
      elevation = srtmelevation,
      shp = shp,
      stats = stats,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  }
}


#' function to compute single raster with zonal
#' @keywords internal

.comp_tri_zonal <- function(elevation = NULL,
                            shp = NULL,
                            stats = "mean",
                            todisk = FALSE,
                            rundir = tempdir,
                            ...) {
  shp_v <- vect(shp)
  rast_mask <- terra::mask(elevation,
    shp_v,
    filename =  ifelse(todisk, file.path(rundir, "elevation.tif"), ""),
    overwrite = TRUE
  )
  p_raster <- terra::rasterize(shp_v,
    rast_mask,
    field = 1:nrow(shp_v),
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
  zstats <- lapply(1:length(stats), function(i) {
    zstats <- terra::zonal(tri,
      p_raster,
      fun = stats[i],
      na.rm = T
    )
    tibble_zstats <- tibble(tri = zstats[, 2])
    names(tibble_zstats)[names(tibble_zstats) == "tri"] <-
      paste0("terrain_ruggedness_index_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}


#' function to compute single raster with terra extract
#' @keywords internal

.comp_tri_extract <- function(elevation = NULL,
                              shp = NULL,
                              stats = "mean",
                              todisk = todisk,
                              rundir = rundir,
                              ...) {
  shp_v <- vect(shp)
  tri <- terra::terrain(elevation,
    v = "TRI",
    unit = "degrees",
    neighbors = 8,
    filename = ifelse(todisk, file.path(rundir, "terrain.tif"), ""),
    overwrite = TRUE
  )
  zstats <- lapply(1:length(stats), function(i) {
    zstats <- terra::extract(tri,
      shp_v,
      fun = stats[i],
      na.rm = T
    )
    tibble_zstats <- tibble(tri = zstats[, 2])
    names(tibble_zstats)[names(tibble_zstats) == "tri"] <-
      paste0("terrain_ruggedness_index_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}


#' function to compute single raster with exactextractr
#' @keywords internal

.comp_tri_exact_extractr <- function(elevation = NULL,
                                     shp = NULL,
                                     stats = "mean",
                                     todisk = todisk,
                                     rundir = rundir,
                                     ...) {
  tri <- terra::terrain(elevation,
    v = "TRI",
    unit = "degrees",
    neighbors = 8,
    filename = ifelse(todisk, file.path(rundir, "terrain.tif"), ""),
    overwrite = TRUE
  )
  zstats <- lapply(1:length(stats), function(i) {
    zstats <- exactextractr::exact_extract(
      tri,
      shp,
      fun = stats[i]
    )
    tibble_zstats <- tibble(tri = zstats)
    names(tibble_zstats)[names(tibble_zstats) == "tri"] <-
      paste0("terrain_ruggedness_index_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}
