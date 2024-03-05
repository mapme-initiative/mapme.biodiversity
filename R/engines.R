available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
available_engines <- c("zonal", "extract", "exactextract")

#' Function to select processing engines
#'
#' `check_engine()` checks if an extraction engine for zonal vector-raster
#' operations is supported by the backend.
#'
#' @param queried_engine A character vector of length one indicating the engine
#'   to check for.
#'
#' @return `check_engine()` returns the character of the queried engine,
#'  if supported. Throws an error otherwise.
#' @keywords utils
#' @name engine
#' @export
check_engine <- function(queried_engine) {
  if (length(queried_engine) > 1) {
    stop(sprintf(
      "Please specify only one engine of: %s.",
      paste(available_engines, collapse = ", ")
    ))
  }
  if (!queried_engine %in% available_engines) {
    stop(sprintf(
      paste("Engine '%s' is not an available engine.",
        "Please choose one of: %s",
        collapse = " "
      ),
      queried_engine, paste(available_engines, collapse = ", ")
    ))
  }
  queried_engine
}

#' Checks if queried statistcs are available
#'
#' `check_stats` checks if one or multiple statistics are supported
#' for zonal vector-raster extraction by the backend.
#'
#' @param queried_stats A character vector with statistic names to be checked
#'   if they are supported by the backend
#'
#' @return `check_stats` returns a character vector of supported statistics.
#'  Throws an error if any of the queried statistics is not supported.
#' @keywords utils
#' @name engine
#' @export
check_stats <- function(queried_stats) {
  if (any(!queried_stats %in% available_stats)) {
    not_available <- queried_stats[which(!queried_stats %in% available_stats)]
    msg_body <- "%s '%s' %s not supported. Please choose one of: %s"
    if (length(not_available) == 1) {
      stat <- "Statistic"
      verb <- "is"
    } else {
      stat <- "Statistics"
      verb <- "are"
    }
    msg <- sprintf(
      msg_body, stat,
      paste(not_available, collapse = "', '"),
      verb,
      paste(available_stats, collapse = ", ")
    )
    stop(msg)
  }
  queried_stats
}

#' Processes statistics with an engine
#'
#' `select_engine` extracts zonal vector-raster statistics for supported engine
#' and for one or more statistics. Columns are named according to the argument
#' `name` plus the respective stat. Both `portfolio` and `asset` modes are supported.
#'
#' @param x An sf object representing a portfolio.
#' @param raster An terra SpatRaster from which values are to be extracted.
#' @param stats A character vector of statistics to aggregate the raster values
#'   with.
#' @param engine A character vector of length one specifying the engine to be
#'   used for the extraction.
#' @param name A character vector indicating the name to append to the columns
#'   names.
#' @param mode A character vector indicating in which mode to conduct the
#'   extraction (e.g. `asset`-wise or for the whole `portfolio` at once).
#' @return `select_engine` returns a tibble.
#' @keywords utils
#' @name engine
#' @export
select_engine <- function(x,
                          raster,
                          stats,
                          engine,
                          name = NULL,
                          mode = "asset") {
  check_stats(stats)
  check_engine(engine)

  engine <- switch(engine,
    "extract" = .engine_extract,
    "exactextract" = .engine_exact_extract,
    "zonal" = .engine_zonal
  )

  if (mode == "asset") {
    result <- engine(x, raster, stats, name)
  } else {
    result <- purrr::map(1:nrow(x), function(i) {
      engine(x[i, ], raster, stats, name)
    })
  }
  result
}

available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
available_engines <- c("zonal", "extract", "exactextract")

.engine_zonal <- function(x, raster, stats, name = NULL) {
  results <- purrr::map_dfc(stats, function(stat) {
    out <- terra::zonal(
      raster,
      vect(x),
      fun = get(stat),
      na.rm = TRUE
    )
    out <- tibble(as.numeric(out))
    names(out) <- ifelse(is.null(name), stat, paste(name, "_", stat, sep = ""))
    out
  })
  results
}

.engine_extract <- function(x, raster, stats, name = NULL) {
  results <- purrr::map_dfc(stats, function(stat) {
    out <- terra::extract(
      raster,
      x,
      fun = get(stat),
      na.rm = TRUE,
      ID = FALSE
    )
    out <- tibble(as.numeric(out))
    names(out) <- ifelse(is.null(name), stat, paste(name, "_", stat, sep = ""))
    out
  })
  results
}

.engine_exact_extract <- function(x, raster, stats, name = NULL) {
  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }

  results <- purrr::map_dfc(stats, function(stat) {
    org_stat <- stat
    if (stat %in% c("sd", "var")) {
      stat <- ifelse(stat == "sd", "stdev", "variance")
    }

    out <- exactextractr::exact_extract(
      raster,
      x,
      fun = stat
    )
    out <- tibble(as.numeric(out))
    names(out) <- ifelse(is.null(name), stat, paste(name, "_", org_stat, sep = ""))
    out
  })
  results
}
