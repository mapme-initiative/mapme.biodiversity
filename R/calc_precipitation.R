#' Calculate precipitation statistics
#'
#' This functions allows to calculate precipitation statistics based on the
#' CHIRPS rainfall estimates. Corresponding to the time-frame of the analysis
#' of the portfolio, monthly precipitation statistics are calculated. Users
#' can decide which statistics to calculate.
#' The required resources for this indicator are:
#'  - \code{chirps}
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats}{The statistic to calculate. One or multiple of: 'mean', 'median', 'sd'.}
#' }
#'
#' @name precipitation
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for years, months, the statistic and the corresponding value (in mm)
NULL

#' Calculate precipitation statistics
#'
#' @param shp A single polygon for which to calculate the tree cover statistic
#' @param chirps The CHIRPS resource
#' @param stats_precipitation A character vector of statistics to calculate
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @importFrom stringr str_sub
#' @keywords internal
#'
.calc_precipitation <- function(shp,
                                chirps,
                                stats_precipitation,
                                engine = "zonal",
                                rundir = tempdir(),
                                verbose = TRUE,
                                todisk = FALSE,
                                ...) {

  # initial argument checks
  # retrieve years from portfolio
  # handling of return value if resources are missing, e.g. no overlap
  if (is.null(chirps)) {
    return(tibble(years = NA, months = NA, stat = NA, precipitation = NA))
  }
  if (ncell(chirps) > 1024 * 1024) todisk <- TRUE
  years <- attributes(shp)$years

  if (any(years < 1981)) {
    warning(paste("Cannot calculate precipitation statistics ",
      "for years smaller than 1981",
      sep = ""
    ))
    years <- years[years >= 1981]
    if (length(years) == 0) {
      return(tibble(years = NA, months = NA, stat = NA, precipitation = NA))
    }
  }

  src_names <- basename(sources(chirps))
  layer_years <- as.numeric(stringr::str_sub(src_names, -11, -8))
  chirps <- chirps[[which(layer_years %in% years)]]

  if (any(!stats_precipitation %in% c("mean", "median", "sd"))) {
    stop("Argument 'stat' for indicator 'chirps' most be one of 'mean', 'median', 'sd'")
  }

  available_engines <- c("zonal", "extract", "exactextract")
  if (!engine %in% available_engines) {
    stop(sprintf("Engine %s is not an available engine. Please choose one of: %s", engine, paste(available_engines, collapse = ", ")))
  }

  if (engine == "extract") {
    .prec_extract(
      chirps = chirps,
      shp = shp,
      stats = stats_precipitation
    )
  } else if (engine == "exactextract") {
    .prec_exact_extractr(
      chirps = chirps,
      shp = shp,
      stats = stats_precipitation
    )
  } else {
    .prec_zonal(
      chirps = chirps,
      shp = shp,
      stats = stats_precipitation,
      todisk = todisk,
      rundir = rundir
    )
  }
}

.prec_zonal <- function(chirps, shp, stats, todisk, rundir) {
  year <- NULL
  shp_v <- vect(shp)
  p_raster <- terra::rasterize(shp_v,
    chirps,
    field = 1,
    touches = TRUE,
    filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    overwrite = TRUE
  )
  chirps[chirps == -9999] <- NA
  results <- lapply(1:length(stats), function(i) {
    out <- terra::zonal(chirps,
      p_raster,
      fun = stats[i],
      na.rm = T
    )
    out <- out[2:ncol(out)]
    names(out) <- stringr::str_replace_all(stringr::str_sub(names(out), -7, -1), "\\.", "-")
    out <- data.frame(precipitation = t(out))
    out <- tibble::rownames_to_column(out, "year")
    out <- tidyr::separate(out, year, into = c("year", "month"), sep = "-")
    out$stat <- stats[i]
    tibble(out[, c(1, 2, 4, 3)])
  })
  results <- do.call(rbind, results)
  results
}


.prec_extract <- function(chirps, shp, stats) {
  year <- NULL
  shp_v <- vect(shp)
  chirps[chirps == -9999] <- NA
  results <- lapply(1:length(stats), function(i) {
    out <- terra::extract(chirps,
      shp_v,
      fun = stats[i],
      na.rm = T
    )
    out <- out[2:ncol(out)]
    names(out) <- stringr::str_replace_all(stringr::str_sub(names(out), -7, -1), "\\.", "-")
    out <- data.frame(precipitation = t(out))
    out <- tibble::rownames_to_column(out, "year")
    out <- tidyr::separate(out, year, into = c("year", "month"), sep = "-")
    out$stat <- stats[i]
    tibble(out[, c(1, 2, 4, 3)])
  })
  results <- do.call(rbind, results)
  results
}


.prec_exact_extractr <- function(chirps, shp, stats) {
  year <- NULL
  calc_stats <- stringr::str_replace_all(stats, "sd", "stdev")
  shp_v <- vect(shp)
  chirps[chirps == -9999] <- NA
  results <- lapply(1:length(stats), function(i) {
    out <- exactextractr::exact_extract(
      chirps,
      shp,
      fun = calc_stats[i]
    )
    names(out) <- stringr::str_replace_all(stringr::str_sub(names(out), -7, -1), "\\.", "-")
    out <- data.frame(precipitation = t(out))
    out <- tibble::rownames_to_column(out, "year")
    out <- tidyr::separate(out, year, into = c("year", "month"), sep = "-")
    out$stat <- stats[i]
    tibble(out[, c(1, 2, 4, 3)])
  })
  results <- do.call(rbind, results)
  results
}
