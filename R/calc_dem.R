#' Calculate elevation statistics
#'
#' This function allows to efficiently calculate elevation statistics for
#' polygons. For each polygon, the desired statistic/s (mean, median or sd)
#' is/are returned.
#' The required resources for this indicator are:
#'  - [srtmdem]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character.  Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name elevation
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for elevation statistics (in meters)
NULL

#' Calculate elevation statistics based on SRTM data sets
#'
#' Considering the 30m resolution SRTM raster datasets users can specify which
#' statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param shp A single polygon for which to calculate the elevation statistic
#' @param srtmdem The elevation raster resource from SRTM
#' @param stats Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character "mean", "median" or "sd".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @noRd

.calc_dem <- function(shp,
                      srtmdem,
                      engine = "zonal",
                      stats_elevation = "mean",
                      rundir = tempdir(),
                      verbose = TRUE,
                      todisk = FALSE,
                      ...) {
  if (is.null(srtmdem)) {
    stat_names <- paste("elevation_", stats_elevation, sep = "")
    out <- tibble(as.data.frame(lapply(1:length(stats_elevation), function(i) NA)))
    names(out) <- stat_names
    return(out)
  }
  # check if input engines are correct
  available_engines <- c("zonal", "extract", "exactextract")
  if (!engine %in% available_engines) {
    stop(sprintf("Engine %s is not an available engine. Please choose one of: %s", engine, paste(available_engines, collapse = ", ")))
  }

  if (ncell(srtmdem) > 1024 * 1024) todisk <- TRUE
  available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
  # check if input stats are correct
  if (!stats_elevation %in% available_stats) {
    stop(sprintf("Stat %s is not an available statistics. Please choose one of: %s", stats_elevation, paste(available_stats, collapse = ", ")))
  }

  if (engine == "extract") {
    tibble_zstats <- .comp_dem_extract(
      elevation = srtmdem,
      shp = shp,
      stats = stats_elevation
    )
    return(tibble_zstats)
  } else if (engine == "exactextract") {
    tibble_zstats <- .comp_dem_exact_extractr(
      elevation = srtmdem,
      shp = shp,
      stats = stats_elevation
    )
    return(tibble_zstats)
  } else {
    tibble_zstats <- .comp_dem_zonal(
      elevation = srtmdem,
      shp = shp,
      stats = stats_elevation,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  }
}

#' Helper function to compute statistics using routines from terra extract
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

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

#' Helper function to compute statistics using routines from terra zonal
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

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

#' Helper function to compute statistics using routines from exactextractr
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_dem_exact_extractr <- function(elevation = NULL,
                                     shp = NULL,
                                     stats = "mean",
                                     ...) {
  if (!"exactextractr" %in% utils::installed.packages()[, 1]) {
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }
  zstats <- lapply(1:length(stats), function(i) {
    if (stats[i] %in% c("sd", "var")) {
      zstats <- exactextractr::exact_extract(
        elevation,
        shp,
        fun = ifelse(stats[i] == "sd", "stdev", "variance")
      )
    } else {
      zstats <- exactextractr::exact_extract(
        elevation,
        shp,
        fun = stats[i]
      )
    }
    tibble_zstats <- tibble(elev = zstats)
    names(tibble_zstats)[names(tibble_zstats) == "elev"] <-
      paste0("elevation_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}
