#' Helper to check for supported resources
#'
#' @param resources A character vector with requested resources
#' @keywords internal
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
  required_resources
}

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

.check_available_years <- function(target_years, available_years, indicator) {
  if (any(!target_years %in% available_years)) {
    message(sprintf("Some target years are not available for %s.", indicator))
    target_years <- target_years[target_years %in% available_years]
    if (length(target_years) == 0) {
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



.download_or_skip <- function(urls,
                              filenames,
                              verbose,
                              stubbornnes = 6,
                              check_existence = TRUE) {
  options(timeout = max(600, getOption("timeout")))
  retry <- TRUE
  counter <- 1
  if (length(urls) < 10) verbose <- FALSE
  while (retry) {
    if (verbose) pb <- progress::progress_bar$new(total = length(urls))
    if (verbose) pb$tick(0)
    unsuccessful <- lapply(seq_along(urls), function(i) {
      if (file.exists(filenames[i])) {
        if (verbose) pb$tick()
        return(NULL) # file exists locally
      }
      if (check_existence) {
        if (!RCurl::url.exists(urls[i])) {
          return(NULL)
        }
      } # file does not exist remotely

      status <- download.file(urls[i], filenames[i], quiet = TRUE, "libcurl")
      if (status != 0) {
        return(list(urls = urls[i], filenames = filenames[i]))
      }

      if (verbose) pb$tick()
      NULL
    })
    counter <- counter + 1

    unsuccessful <- unsuccessful[which(sapply(unsuccessful, function(x) !is.null(x)))]
    if (length(unsuccessful) > 0 & counter <= stubbornnes) {
      warning(paste("Some target files have not been downloaded correctly. ",
        "Download will be retried.",
        sep = ""
      ))
      urls <- sapply(unsuccessful, function(x) x$urls)
      filenames <- sapply(unsuccessful, function(x) x$filenames)
    }
    if (counter > stubbornnes | length(unsuccessful) == 0) retry <- FALSE
  }
}
