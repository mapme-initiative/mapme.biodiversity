available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
available_engines <- c("zonal", "extract", "exactextract")

.check_engine <- function(queried_engine) {
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
}

.check_stats <- function(queried_stats) {
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
}

.select_engine <- function(x, raster, stats, engine, name = NULL, mode = "asset") {
  .check_stats(stats)
  .check_engine(engine)

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
