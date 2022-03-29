#' Calculate Terrain Ruggedness Index (TRI) statistics
#'
#' Terrain Ruggedness Index is a measurement developed by Riley, et al. (1999).
#' The elevation difference between the center pixel and its eight immediate
#' pixels are squared and then averaged and its square root is taken to get
#' the TRI value. This function allows to efficiently calculate terrain ruggedness
#' index (tri) statistics for polygons. For each polygon, the desired statistic/s
#' (mean, median or sd) is/are returned.
#' The required resources for this indicator are:
#'  - \code{srtmdem}
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats}{Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name tri
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for terrain ruggedness index statistics (in meters).
#'   The range of index values and corresponding meaning:
#'   (1) 0 - 80 m  :- level surface
#'   (2) 81-116 m  :- nearly level surface
#'   (3) 117-161 m :- slightly rugged surface
#'   (4) 162-239 m :- intermediately rugged surface
#'   (5) 240-497 m :- moderately rugged surface
#'   (6) 498-958 m :- highly rugged surface
#'   (7) 959-4367 m:- extremely rugged surface
#' @references Riley, S. J., DeGloria, S. D., & Elliot, R. (1999). Index that quantifies
#'   topographic heterogeneity. intermountain Journal of sciences, 5(1-4), 23-27.
NULL

#' Calculate Terrain Ruggedness Index (TRI) statistics based on SRTM data sets
#'
#' Considering the 30m resolution SRTM raster datasets users can specify which
#' statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param shp A single polygon for which to calculate the tri statistic
#' @param srtmdem The elevation raster resource from SRTM
#' @param stats_tri Function to be applied to compute statistics for polygons either
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

.calc_tri <- function(shp,
                      srtmdem,
                      engine = "zonal",
                      stats_tri = "mean",
                      rundir = tempdir(),
                      verbose = TRUE,
                      todisk = FALSE,
                      ...) {
  if (is.null(srtmdem)) {
    stat_names <- paste("terrain_ruggedness_index_", stats_tri, sep = "")
    out <- tibble(as.data.frame(lapply(1:length(stats_tri), function(i) NA)))
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
  if (!any(stats_tri %in% available_stats)) {
    stop(sprintf("Stat %s is not an available statistics. Please choose one of: %s", stats_tri, paste(available_stats, collapse = ", ")))
  }

  if (engine == "extract") {
    tibble_zstats <- .comp_tri_extract(
      elevation = srtmdem,
      shp = shp,
      stats = stats_tri,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  } else if (engine == "exactextract") {
    tibble_zstats <- .comp_tri_exact_extractr(
      elevation = srtmdem,
      shp = shp,
      stats = stats_tri,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  } else {
    tibble_zstats <- .comp_tri_zonal(
      elevation = srtmdem,
      shp = shp,
      stats = stats_tri,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  }
}


#' Helper function to compute statistics using routines from terra zonal
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

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


#' Helper function to compute statistics using routines from terra extract
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

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


#' Helper function to compute statistics using routines from exactextractr
#'
#' @param elevation elevation raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_tri_exact_extractr <- function(elevation = NULL,
                                     shp = NULL,
                                     stats = "mean",
                                     todisk = todisk,
                                     rundir = rundir,
                                     ...) {
  if (!"exactextractr" %in% utils::installed.packages()[, 1]) {
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }
  tri <- terra::terrain(elevation,
    v = "TRI",
    unit = "degrees",
    neighbors = 8,
    filename = ifelse(todisk, file.path(rundir, "terrain.tif"), ""),
    overwrite = TRUE
  )
  zstats <- lapply(1:length(stats), function(i) {
    if (stats[i] %in% c("sd", "var")) {
      zstats <- exactextractr::exact_extract(
        tri,
        shp,
        fun = ifelse(stats[i] == "sd", "stdev", "variance")
      )
    } else {
      zstats <- exactextractr::exact_extract(
        tri,
        shp,
        fun = stats[i]
      )
    }
    tibble_zstats <- tibble(tri = zstats)
    names(tibble_zstats)[names(tibble_zstats) == "tri"] <-
      paste0("terrain_ruggedness_index_", stats[i])
    return(tibble_zstats)
  })
  unlist_zstats <- do.call(cbind, zstats)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}
