#' Calculate elevation based on SRTM rasters
#'

.calc_dem <- function(shp,
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

    tibble_zstats <- .comp_dem_extract(
      elevation = srtmelevation,
      shp = shp,
      stats = stats
    )
    return(tibble_zstats)
  } else if (engine == "exactextract") {

    # check if input stats are correct
    if (!stats %in% available_exact_stats) {
      stop(sprintf("Stat %s is not an available statistics. Please choose one of: %s", stats, paste(available_exact_stats, collapse = ", ")))
    }

    tibble_zstats <- .comp_dem_exact_extractr(
      elevation = srtmelevation,
      shp = shp,
      stats = stats
    )
    return(tibble_zstats)
  } else {

    # check if input stats are correct
    if (!stats %in% available_terra_stats) {
      stop(sprintf("Stat %s is not an available statistics. Please choose one of: %s", stats, paste(available_terra_stats, collapse = ", ")))
    }

    tibble_zstats <- .comp_dem_zonal(
      elevation = srtmelevation,
      shp = shp,
      stats = stats,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  }
}


#' function to compute single raster with terra extract
#'

.comp_dem_extract <- function(elevation = NULL,
                              shp = NULL,
                              stats = "mean",
                              ...) {
  shp_v <- vect(shp)
  zstats <- lapply(1:length(stats), function(i) {
    zstats <- terra::extract(elevation,
      shp_v,
      fun = stats[i],
      na.rm = T
    )
    tibble_zstats <- tibble(elev = zstats[, 2])
    names(tibble_zstats)[names(tibble_zstats) == "elev"] <-
      paste0("elevation_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}


#' function to compute single raster with terra zonal
#'

.comp_dem_zonal <- function(elevation = NULL,
                            shp = NULL,
                            stats = "mean",
                            todisk = FALSE,
                            rundir = tempdir(),
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
  zstats <- lapply(1:length(stats), function(i) {
    zstats <- terra::zonal(rast_mask,
      p_raster,
      fun = stats[i],
      na.rm = T
    )
    tibble_zstats <- tibble(elev = zstats[, 2])
    names(tibble_zstats)[names(tibble_zstats) == "elev"] <-
      paste0("elevation_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}


#' function to compute single raster with exactextractr
#'

.comp_dem_exact_extractr <- function(elevation = NULL,
                                     shp = NULL,
                                     stats = "mean",
                                     ...) {
  zstats <- lapply(1:length(stats), function(i) {
    zstats <- exactextractr::exact_extract(
      elevation,
      shp,
      fun = stats[i]
    )
    tibble_zstats <- tibble(elev = zstats)
    names(tibble_zstats)[names(tibble_zstats) == "elev"] <-
      paste0("elevation_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}
