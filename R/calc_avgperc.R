#' Calculate long-term average precipitation
#'
#' This functions allows to calculate the monthly long-term average precipitation
#' based on the CHIRPS data set. The long-term average is defined by the monthly
#' average precipitation during the climate normal period 1981 - 2010. The average
#' value of a CHIRPS pixels that intersect with a given polygon are returned.
#' Users do not need to supply further arguments.
#' The required resources for this indicator are:
#'  - \code{chirps}
#'
#' @name avgperc
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for years, months, the statistic and the corresponding value (in mm)
NULL

#' Calculate long-term precipitation monthly averages
#' @param shp A single polygon for which to calculate the tree cover statistic
#' @param chirps The CHIRPS resource
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @importFrom stringr str_sub
#' @keywords internal
#'
.calc_avgperc <- function(shp,
                          chirps,
                          engine = "zonal",
                          rundir = tempdir(),
                          verbose = TRUE,
                          todisk = FALSE,
                          ...) {

  # initial argument checks
  # handling of return value if resources are missing, e.g. no overlap
  if (is.null(chirps)) {
    results <- as.data.frame(lapply(1:12, function(i) NA))
    names(results) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    return(tibble(results))
  }

  # retrieve years from portfolio
  if (ncell(chirps) > 1024 * 1024) todisk <- TRUE

  available_engines <- c("zonal", "extract", "exactextract")
  if (!engine %in% available_engines) {
    stop(sprintf("Engine %s is not an available engine. Please choose one of: %s", engine, paste(available_engines, collapse = ", ")))
  }

  unique_vals <- unique(as.vector(values(chirps)))
  if (length(unique_vals) == 1) {
    if (is.na(unique_vals)) {
      results <- as.data.frame(lapply(1:12, function(i) NA))
      names(results) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      return(tibble(results))
    }
  }

  src_names <- basename(sources(chirps))
  layer_years <- as.numeric(stringr::str_sub(src_names, -11, -8))
  chirps <- chirps[[which(layer_years %in% 1981:2010)]]
  layer_names <- names(chirps)
  layer_months <- as.numeric(stringr::str_sub(layer_names, -2, -1))
  chirps_monthly <- lapply(1:12, function(i) {
    chirps[[layer_months == i]]
  })

  if (engine == "extract") {
    .avgprec_extract(
      chirps_monthly = chirps_monthly,
      shp = shp,
      rundir = rundir,
      todisk = todisk
    )
  } else if (engine == "exactextract") {
    .avgprec_exact_extractr(
      chirps_monthly = chirps_monthly,
      shp = shp,
      rundir = rundir,
      todisk = todisk
    )
  } else {
    .avgprec_zonal(
      chirps_monthly = chirps_monthly,
      shp = shp,
      todisk = todisk,
      rundir = rundir
    )
  }
}

.avgprec_zonal <- function(shp, chirps_monthly, rundir, todisk) {
  shp_v <- vect(shp)
  p_raster <- terra::rasterize(shp_v,
    chirps_monthly[[1]],
    field = 1,
    touches = TRUE,
    filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    overwrite = TRUE
  )
  results <- lapply(1:length(chirps_monthly), function(i) {
    tmp <- mean(chirps_monthly[[i]],
      filename = ifelse(todisk, file.path(rundir, "mean_chirps.tif"), "")
    )
    out <- terra::zonal(tmp,
      p_raster,
      fun = mean,
      na.rm = T
    )
    out[2]
  })
  results <- as.data.frame(results)
  names(results) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  tibble(results)
}


.avgprec_extract <- function(shp, chirps_monthly, rundir, todisk) {
  year <- NULL
  shp_v <- vect(shp)
  results <- lapply(1:length(chirps_monthly), function(i) {
    tmp <- mean(chirps_monthly[[i]],
      filename = ifelse(todisk, file.path(rundir, "mean_chirps.tif"), "")
    )
    out <- terra::extract(tmp,
      shp_v,
      fun = mean,
      na.rm = T
    )
    out[2]
  })
  results <- as.data.frame(results)
  names(results) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  tibble(results)
}


.avgprec_exact_extractr <- function(shp, chirps_monthly, rundir, todisk) {
  results <- lapply(1:length(chirps_monthly), function(i) {
    tmp <- mean(chirps_monthly[[i]],
      filename = ifelse(todisk, file.path(rundir, "mean_chirps.tif"), "")
    )
    out <- exactextractr::exact_extract(tmp,
      shp,
      fun = "mean"
    )
    out
  })
  results <- as.data.frame(results)
  names(results) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  tibble(results)
}
