#' Helper to check for supported resources
#'
#' @param resources A character vector with requested resources
#' @keywords internal
#' @noRd
.check_requested_resources <- function(resources) {
  names_resources <- names(available_resources())
  # check for unsupported resources
  if (any(!resources %in% names_resources)) {
    unsupported <- resources[which(!resources %in% names_resources)]
    base_msg <- "The following requested %s not supported: %s."
    mid_msg <- ifelse(length(unsupported) == 1, "resource is", "resources are")
    end_msg <- paste(unsupported, collapse = ", ")
    stop(sprintf(base_msg, mid_msg, end_msg))
  }
}

#' Helper to check for supported indicators
#'
#' @param indicators A character vector with requested indicators
#' @keywords internal
#' @noRd
.check_requested_indicator <- function(indicators) {
  names_indicators <- names(available_indicators())
  # check for unsupported resources
  if (any(!indicators %in% names_indicators)) {
    unsupported <- indicators[which(!indicators %in% names_indicators)]
    base_msg <- "The following requested %s not supported: %s."
    mid_msg <- ifelse(length(unsupported) == 1,
      "indicator is", "indicators are"
    )
    end_msg <- paste(unsupported, collapse = ", ")
    stop(sprintf(base_msg, mid_msg, end_msg))
  }
  required_resources <- sapply(
    available_indicators()[indicators],
    function(x) names(x$inputs)
  )
  required_resources <- unique(unlist(required_resources))
  as.vector(required_resources)
}

#' Helper to check for existing resources
#'
#' @param ex_resources A character vector with existing resources
#' @param req_resources A character vector with requested resources
#' @param needed default value being FALSE
#' @keywords internal
#' @noRd
.check_existing_resources <- function(ex_resources,
                                      req_resources,
                                      needed = FALSE) {
  if (needed == FALSE) {
    if (any(req_resources %in% ex_resources)) {
      existing <- req_resources[which(req_resources %in% ex_resources)]
      nonexisting <- req_resources[which(!req_resources %in% ex_resources)]
      base_msg <- "The following requested %s already available: %s."
      mid_msg <- ifelse(length(existing) == 1, "resource is", "resources are")
      end_msg <- paste(existing, collapse = ", ")
      message(sprintf(base_msg, mid_msg, end_msg))
      nonexisting
    } else {
      req_resources
    }
  } else {
    if (any(!req_resources %in% ex_resources)) {
      existing <- req_resources[which(req_resources %in% ex_resources)]
      nonexisting <- req_resources[which(!req_resources %in% ex_resources)]
      base_msg <- "The following required %s not available: %s."
      mid_msg <- ifelse(length(nonexisting) == 1,
        "resource is",
        "resources are"
      )
      end_msg <- paste(nonexisting, collapse = ", ")
      stop(sprintf(base_msg, mid_msg, end_msg))
      nonexisting
    } else {
      NULL
    }
  }
}



#' Helper to check for resource arguments
#'
#' @param resource A character vector with requested resource
#' @param args The arguments of requested resource
#' @keywords internal
#' @noRd
.check_resource_arguments <- function(resource, args) {
  # TODO: What about portfolio wide parameters?
  resource_name <- names(resource)
  required_args <- resource[[1]]$arguments
  specified_args <- args[names(args) %in% names(required_args)]
  # return early if all required arguments have been specified,
  # note that the correctness of the values have to be checked in the resource
  # function
  if (length(specified_args) == length(required_args)) {
    return(specified_args)
  }
  if (length(specified_args) == 0) unspecified_args <- names(required_args)
  if (length(specified_args) > 0) {
    req_args_names <- names(required_args)
    unspecified_args <- req_args_names[!req_args_names %in% names(args)]
  }
  base_msg <- paste("Argument '%s' for resource '%s' was not specified. ",
    "Setting to default value of '%s'.",
    sep = ""
  )
  default_args <- as.list(sapply(unspecified_args, function(arg_name) {
    message(
      sprintf(
        base_msg, arg_name, resource_name,
        paste0(required_args[[arg_name]], collapse = ", ")
      )
    )
    required_args[[arg_name]]
  }))

  if (length(specified_args) > 0) {
    append(specified_args, default_args)
  } else {
    default_args
  }
}


#' Helper to create global grid
#'
#' @param xmin minimum longitude value (E/W)
#' @param xmax maximum longitude value (E/W)
#' @param ymin minimum latitude value (S/N)
#' @param ymax maximum latitude value (E/W)
#' @param dx difference in longitude value per grid
#' @param dy difference in latitude value per grid
#' @param proj projection system
#' @keywords internal
#' @noRd
.make_global_grid <- function(xmin = -180, xmax = 170, dx = 10,
                              ymin = -50, ymax = 80, dy = 10,
                              proj = NULL) {
  if (is.null(proj)) proj <- st_crs(4326)
  ncells <- c(
    (xmax - xmin) / dx,
    (ymax - ymin) / dy
  )

  bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin))
  st_as_sf(st_make_grid(bbox, n = ncells, crs = "EPSG:4326", what = "polygons"))
}

.get_gfw_tile_id <- function(tile) {
  min_x <- st_bbox(tile)[1]
  max_y <- st_bbox(tile)[4]

  # prepare tile names
  if (min_x < 0) {
    min_x <- paste0(sprintf("%03i", abs(min_x)), "W")
  } else {
    min_x <- paste0(sprintf("%03i", min_x), "E")
  }
  if (max_y < 0) {
    max_y <- paste0(sprintf("%02i", abs(max_y)), "S")
  } else {
    max_y <- paste0(sprintf("%02i", max_y), "N")
  }

  paste0(max_y, "_", min_x)
}


#' Helper to unzip and remove zip files
#'
#' @param zip zip file to unzip
#' @param rundir A directory where intermediate files are written to.
#' @param remove if TRUE, removes the zip else keeps it
#' @keywords internal
#' @noRd
.unzip_and_remove <- function(zip, rundir, remove = TRUE) {
  suppressWarnings(unzip(
    zipfile = file.path(rundir, basename(zip)),
    exdir = rundir,
    overwrite = FALSE
  ))
  if (remove) {
    unlink(file.path(rundir, basename(zip)))
  }
}


#' Helper to check year availability
#'
#' @param target_years Numeric/s indicating the target year/s
#' @param available_years Numeric/s indicating the available year/s
#' @param indicator A character vector with target indicator
#' @keywords internal
#' @noRd
.check_available_years <- function(target_years, available_years, indicator) {
  if (any(!target_years %in% available_years)) {
    target_years <- target_years[target_years %in% available_years]
    if(length(target_years) > 0 ){
    message(sprintf("Some target years are not available for %s.", indicator))
    } else {
      stop(
        sprintf(
          "The target years do not intersect with the availability of %s.",
          indicator
        )
      )
    }
  }
  target_years
}

.check_engine <- function(implemented_engines, queried_engine) {
  if (length(queried_engine) > 1) {
    stop(sprintf(
      "Please specify only one engine of: %s.",
      paste(implemented_engines, collapse = ", ")
    ))
  }

  if (!queried_engine %in% implemented_engines) {
    stop(sprintf(
      paste("Engine '%s' is not an available engine.",
        "Please choose one of: %s",
        collapse = " "
      ),
      queried_engine, paste(implemented_engines, collapse = ", ")
    ))
  }
}

.check_stats <- function(implemented_stats, queried_stats) {
  if (any(!queried_stats %in% implemented_stats)) {
    not_available <- queried_stats[which(!queried_stats %in% implemented_stats)]
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
      paste(implemented_stats, collapse = ", ")
    )
    stop(msg)
  }
}


#' Helper to check valid urls
#'
#' @param urls address to the datasource
#' @param filenames name of the files in the url
#' @param verbose Logical controlling verbosity.
#' @param stubbornnes default value being 6
#' @param check_existence default to TRUE
#' @keywords internal
#' @noRd
.download_or_skip <- function(urls,
                              filenames,
                              verbose,
                              stubbornnes = 6,
                              check_existence = TRUE,
                              aria_bin = NULL) {
  if (check_existence) {
    if (verbose) message("Checking URLs for existence. This may take a while...")
    url_exists <- unlist(lapply(urls, function(url) !httr::http_error(url)))
    urls <- urls[url_exists]
    filenames <- filenames[url_exists]
  }

  exists_index <- which(file.exists(filenames))
  if (length(exists_index) > 0) {
    if (verbose) message("Skipping existing files in output directory.")
    missing_filenames <- filenames[-exists_index]
    missing_urls <- urls[-exists_index]
  } else {
    missing_filenames <- filenames
    missing_urls <- urls
  }

  if (length(missing_filenames) == 0) {
    return(filenames)
  }

  if (is.null(aria_bin)) {
    options(timeout = max(600, getOption("timeout")))
    retry <- TRUE
    counter <- 1
    while (retry) {
      unsuccessful <- pbapply::pblapply(seq_along(missing_urls), function(i) {
        if (file.exists(missing_filenames[i])) {
          return(NULL) # file exists locally
        }

        status <- download.file(missing_urls[i], missing_filenames[i],
          quiet = TRUE, "libcurl",
          mode = ifelse(Sys.info()["sysname"] == "Windows", "wb", "w")
        )
        if (status != 0) {
          return(list(urls = missing_urls[i], filenames = missing_filenames[i]))
        }
        NULL
      })
      counter <- counter + 1

      unsuccessful <- unsuccessful[which(sapply(unsuccessful, function(x) !is.null(x)))]
      if (length(unsuccessful) > 0 & counter <= stubbornnes) {
        warning(paste("Some target files have not been downloaded correctly. ",
          "Download will be retried.",
          sep = ""
        ))
        missing_urls <- sapply(unsuccessful, function(x) x$missing_urls)
        missing_filenames <- sapply(unsuccessful, function(x) x$missing_filenames)
      }
      if (counter > stubbornnes | length(unsuccessful) == 0) retry <- FALSE
    }
  } else { # use aria_bin

    outdir <- dirname(missing_filenames[1])
    missing_filenames <- basename(missing_filenames)
    lines <- lapply(1:length(missing_urls), function(i) {
      c(missing_urls[i], paste0("  out=", missing_filenames[i]))
    })
    lines <- unlist(lines)
    tmpfile <- tempfile()
    writeLines(lines, tmpfile)
    if (verbose) {
      args <- sprintf(
        "--show-console-readout=false --console-log-level=warn -c -j 8 -i %s -d %s",
        tmpfile, outdir
      )
    } else {
      args <- sprintf(
        "--quiet -c -j 8 -i %s -d %s",
        tmpfile, outdir
      )
    }
    out <- system2("/bin/aria2c", args = args)
    file.remove(tmpfile)
  }
  return(filenames)
}
