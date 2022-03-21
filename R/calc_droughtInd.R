#' Calculate drought indicator statistics
#'
#' This function allows to efficiently calculate drought indicator statistics for
#' polygons. For each polygon, the desired statistic/s (mean, median or sd) is/are
#' returned. The required resources for this indicator are:
#'  - \code{droughtindicators}
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_drought}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character "mean", "median" or "sd".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name drought_indicator
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for drought indicator statistics (in percentile)
NULL

#' Calculate drought indicator statistics
#'
#' Considering the 0.25 degrees drought indicator raster datasets users can specify
#' which statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param shp A single polygon for which to calculate the drought statistic
#' @param droughtindicators The drought indicator raster resource from NASA GRACE
#' @param stats_drought Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "mean", "median" or "sd".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#'

.calc_drought_indicator <- function(shp,
                                    droughtindicators,
                                    engine = "zonal",
                                    stats_drought = "mean",
                                    rundir = tempdir(),
                                    verbose = TRUE,
                                    todisk = FALSE,
                                    ...) {

  # check if input engines are correct
  available_engines <- c("zonal", "extract", "exactextract")
  if (!engine %in% available_engines) {
    stop(sprintf("Engine %s is not an available engine. Please choose one of: %s", engine, paste(available_engines, collapse = ", ")))
  }

  if (ncell(droughtindicators) > 1024 * 1024) todisk <- TRUE
  available_stats <- c("mean", "median", "sd")
  # check if input stats are correct
  if (!stats_drought %in% available_stats) {
    stop(sprintf("Stat %s is not an available statistics. Please choose one of: %s", stats_drought, paste(available_stats, collapse = ", ")))
  }

  if (engine == "extract") {
    tibble_zstats <- .comp_drought_extract(
      shp = shp,
      drought_rast = droughtindicators,
      stats = stats_drought
    )
    return(tibble_zstats)
  } else if (engine == "exactextract") {
    tibble_zstats <- .comp_drought_exact_extract(
      shp = shp,
      drought_rast = droughtindicators,
      stats = stats_drought
    )
    return(tibble_zstats)
  } else {
    tibble_zstats <- .comp_drought_zonal(
      drought_rast = droughtindicators,
      shp = shp,
      stats = stats_drought,
      todisk = todisk,
      rundir = rundir
    )
    return(tibble_zstats)
  }
}

#' Helper function to compute statistics using routines from terra zonal
#'
#' @param drought_rast drought indicator raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#'

.comp_drought_zonal <- function(drought_rast = NULL,
                                shp = NULL,
                                stats = "mean",
                                todisk = FALSE,
                                rundir = tempdir(),
                                ...) {
  shp_v <- vect(shp)
  data <- lapply(1:nlyr(drought_rast), function(i) {
    pre_data <- lapply(1:length(stats), function(j) {
      rast_mask <- terra::mask(drought_rast[[i]],
        shp_v,
        filename =  ifelse(todisk, file.path(rundir, "droughtindicators.tif"), ""),
        overwrite = TRUE
      )
      p_raster <- terra::rasterize(shp_v,
        rast_mask,
        field = 1:nrow(shp_v),
        touches = TRUE,
        filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
        overwrite = TRUE
      )
      zstats <- terra::zonal(
        rast_mask,
        p_raster,
        fun = stats[j],
        na.rm = T
      )
      tibble1 <- tibble(drought = zstats[, 2])
      bn <- basename(sources(drought_rast[[i]]))
      time_frame <- sub(".*(\\d{8}).*", "\\1", bn)
      names(tibble1)[names(tibble1) == "drought"] <-
        paste0("drought_", stats[j], "_", time_frame)
      return(tibble1)
    })
    unlist_zstats <- do.call(cbind, pre_data)
    tibble2 <- tibble(unlist_zstats)
    return(tibble2)
  })
  unlist_zstats <- do.call(cbind, data)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}

#' Helper function to compute statistics using routines from terra extract
#'
#' @param drought_rast drought indicator raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#'

.comp_drought_extract <- function(shp = NULL,
                                  drought_rast = NULL,
                                  stats = "mean",
                                  ...) {
  shp_v <- vect(shp)
  data <- lapply(1:nlyr(drought_rast), function(i) {
    pre_data <- lapply(1:length(stats), function(j) {
      zstats <- terra::extract(
        drought_rast[[i]],
        shp_v,
        fun = stats[j],
        na.rm = T
      )
      tibble1 <- tibble(drought = zstats[, 2])
      bn <- basename(sources(drought_rast[[i]]))
      time_frame <- sub(".*(\\d{8}).*", "\\1", bn)
      names(tibble1)[names(tibble1) == "drought"] <-
        paste0("drought_", stats[j], "_", time_frame)
      return(tibble1)
    })
    unlist_zstats <- do.call(cbind, pre_data)
    tibble2 <- tibble(unlist_zstats)
    return(tibble2)
  })
  unlist_zstats <- do.call(cbind, data)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}

#' Helper function to compute statistics using routines from exactextractr
#'
#' @param drought_rast drought indicator raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#'

.comp_drought_exact_extract <- function(shp = NULL,
                                        drought_rast = NULL,
                                        stats = "mean",
                                        ...) {
  if (!"exactextractr" %in% utils::installed.packages()[, 1]) {
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }

  data <- lapply(1:nlyr(drought_rast), function(i) {
    pre_data <- lapply(1:length(stats), function(j) {
      if (stats[j] == "sd") {
        zstats <- exactextractr::exact_extract(
          drought_rast[[i]],
          shp,
          fun = "stdev"
        )
      } else {
        zstats <- exactextractr::exact_extract(
          drought_rast[[i]],
          shp,
          fun = stats[j]
        )
      }

      tibble1 <- tibble(drought = zstats)
      bn <- basename(sources(drought_rast[[i]]))
      time_frame <- sub(".*(\\d{8}).*", "\\1", bn)
      names(tibble1)[names(tibble1) == "drought"] <-
        paste0("drought_", stats[j], "_", time_frame)
      return(tibble1)
    })
    unlist_zstats <- do.call(cbind, pre_data)
    tibble2 <- tibble(unlist_zstats)
    return(tibble2)
  })
  unlist_zstats <- do.call(cbind, data)
  tibble_zstats <- tibble(unlist_zstats)
  return(tibble_zstats)
}
