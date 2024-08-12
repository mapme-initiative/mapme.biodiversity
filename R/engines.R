available_stats <- c("mean", "median", "sd", "stdev", "min", "max", "sum", "var", "variance")
#' Function to select processing engines
#'
#' This function is deprecated and will be removed in a future release.
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
check_engine <- function(queried_engine = NULL) {
  if (!is.null(queried_engine)) {
    warning("engine argument is deprecated and will be removed in a future release.")
  }
  NULL
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
#' @param engine Deprecated. Will be removed in a future release.
#' @param name A character vector indicating the name to append to the columns
#'   names.
#' @param mode A character vector indicating in which mode to conduct the
#'   extraction (e.g. `asset`-wise or for the whole `portfolio` at once).
#' @param verbose A logical, indicating if informative messages should be printed.
#' @return `select_engine` returns a tibble.
#' @keywords utils
#' @name engine
#' @export
select_engine <- function(x,
                          raster,
                          stats,
                          engine = NULL,
                          name = NULL,
                          mode = "asset",
                          verbose = mapme_options()[["verbose"]]) {
  check_stats(stats)
  check_engine(engine)

  engine <- .engine_exact_extract

  if (mode == "asset") {
    result <- engine(x, raster, stats, name)
  } else {
    report_progress <- verbose && check_namespace("progressr", error = FALSE)
    step <- .calc_steps(x)
    if (report_progress) {
      p <- progressr::progressor(min(nrow(x), 100))
    }
    raster <- terra::wrap(raster, proxy = TRUE)
    result <- furrr::future_map(1:nrow(x), function(i) {
      out <- engine(x[i, ], terra::unwrap(raster), stats, name)
      if (report_progress) {
        if (i %% step == 0) {
          p()
        }
      }
      out
    }, .options = furrr::furrr_options(seed = TRUE, chunk_size = step))
    result
  }
}

.engine_exact_extract <- function(x, raster, stats, name = NULL) {
  stats <- gsub("var$", "variance", stats)
  stats <- gsub("sd$", "stdev", stats)

  data <- exactextractr::exact_extract(raster, x, fun = stats, force_df = TRUE)

  names(data) <- sapply(names(data), function(y) strsplit(y, "\\.")[[1]][1])
  if (!is.null(name)) {
    names(data) <- paste(name, "_", names(data), sep = "")
  }
  as_tibble(split(unname(unlist(data)), names(data)))
}
