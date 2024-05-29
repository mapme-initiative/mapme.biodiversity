#' Helper to create a grid of regular resolution and CRS
#'
#' Use this function to create a regular grid in a custom CRS. This is used
#' e.g. to create the tile grid for Global Forest Watch in order to retrieve
#' the intersecting tiles with a given portfolio.
#'
#' @param xmin minimum longitude value (E/W)
#' @param xmax maximum longitude value (E/W)
#' @param ymin minimum latitude value (S/N)
#' @param ymax maximum latitude value (E/W)
#' @param dx difference in longitude value per grid
#' @param dy difference in latitude value per grid
#' @param proj projection system
#' @returns An sf object with a defined grid.
#' @keywords utils
#' @export
make_global_grid <- function(xmin = -180, xmax = 170, dx = 10,
                             ymin = -50, ymax = 80, dy = 10,
                             proj = NULL) {
  if (is.null(proj)) proj <- st_crs(4326)
  ncells <- c(
    (xmax - xmin) / dx,
    (ymax - ymin) / dy
  )

  bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin))
  bbox <- st_as_sfc(bbox, crs = proj)
  st_as_sf(st_make_grid(bbox, n = ncells, crs = proj, what = "polygons"))
}

#' Helper to unzip and remove zip/gzip files
#'
#' Use this function to unzip a zip/gzip file and remove the original
#' archive, if required.
#'
#' @param zip zip file to unzip
#' @param dir A directory where the extracted files are written to.
#' @param remove if TRUE, removes the zip else keeps it
#' @keywords utils
#' @export
unzip_and_remove <- function(zip = NULL,
                             dir = tempdir(),
                             remove = TRUE) {
  extension <- tools::file_ext(zip)
  if (extension == "zip") {
    filenames <- suppressWarnings(unzip(
      zipfile = zip,
      exdir = dir,
      overwrite = FALSE
    ))
  } else if (extension == "gz") {
    filenames <- R.utils::gunzip(
      zip,
      skip = TRUE,
      remove = FALSE
    )
  } else {
    stop(paste0("decompression for ", extension, " files not implemented"))
  }
  if (remove) {
    unlink(file.path(dir, basename(zip)))
  }
  return(filenames)
}


#' Helper to check yearly availability
#'
#' Use this function to check if a specifed vector of years intersects
#' with the yearly availablity of a resource.
#'
#' @param target_years Numeric indicating the target year.
#' @param available_years Numeric indicating the available years.
#' @param indicator A character vector with target resource/indicator name.
#' @keywords utils
#' @export
check_available_years <- function(target_years,
                                  available_years,
                                  indicator) {
  if (any(!target_years %in% available_years)) {
    target_years <- target_years[target_years %in% available_years]
    if (length(target_years) > 0) {
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

#' Helper to check and download urls
#'
#' Use this function to fetch a number of remote URLs to local files.
#' In case of unreliable source servers, failed downlaods are retried up to
#' the number of times specified with `stubbornness`. A path to an aria2c
#' executable can be specified to use it instead of the default R download
#' function.
#'
#' @param urls A character vector with URLs to be downloaded.
#' @param filenames A character vector with local file paths the same length
#'   as `urls`
#' @param verbose A logical controlling the verbosity.
#' @param stubbornness A numeric indicating the number of retries for failed
#'   downloads.
#' @param check_existence A logical indicating if `urls` are to be checked
#'   before trying to download. Defaults to TRUE.
#' @param aria_bin A character vector pointing towards an aria2c executable.
#' @keywords utils
#' @export
download_or_skip <- function(urls = NULL,
                             filenames = NULL,
                             verbose = mapme_options()[["verbose"]],
                             stubbornness = 6,
                             check_existence = TRUE,
                             aria_bin = mapme_options()[["aria_bin"]]) {
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
      unsuccessful <- purrr::map(seq_along(missing_urls), function(i) {
        if (file.exists(missing_filenames[i])) {
          return(NULL) # file exists locally
        }

        status <- download.file(missing_urls[i], missing_filenames[i],
          quiet = TRUE,
          mode = ifelse(Sys.info()["sysname"] == "Windows", "wb", "w")
        )
        if (status != 0) {
          return(list(urls = missing_urls[i], filenames = missing_filenames[i]))
        }
        NULL
      })
      counter <- counter + 1

      unsuccessful <- unsuccessful[which(sapply(unsuccessful, function(x) !is.null(x)))]
      if (length(unsuccessful) > 0 & counter <= stubbornness) {
        warning(paste("Some target files have not been downloaded correctly. ",
          "Download will be retried.",
          sep = ""
        ))
        missing_urls <- sapply(unsuccessful, function(x) x$missing_urls)
        missing_filenames <- sapply(unsuccessful, function(x) x$missing_filenames)
      }
      if (counter > stubbornness | length(unsuccessful) == 0) retry <- FALSE
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


#' Checks if namespace is available
#'
#' Use this function if your resource/indicator function requires the
#' namespace of a certain package to be available. An informative error/warning
#' message is printed if that is not the case.
#'
#' @param pkg A character vector of length one indicating a package name
#'   for which the namespace is tested
#' @param error A logical indicating whether or not to promote missing namespace
#'   to error. If FALSE, a warning is emitted.
#'
#' @return TRUE, invisible, if the namespace is available. An error message
#'   if `error = TRUE`, FALSE and a warning otherwise.
#' @keywords utils
#' @export
check_namespace <- function(pkg, error = TRUE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste("R package '%s' required.\n",
      "Please install via `install.packages('%s')`",
      sep = ""
    )
    msg <- sprintf(msg, pkg, pkg)
    if (error) {
      stop(msg, .call = FALSE)
    } else {
      warning(msg, .call = FALSE)
      return(invisible(FALSE))
    }
  }
  invisible(TRUE)
}

#' @importFrom httr http_error
.has_internet <- function() {
  if (Sys.getenv("mapme_check_connection", unset = "TRUE") == "FALSE") {
    return(TRUE)
  }
  has_internet <- !httr::http_error("www.google.com")
  if (!has_internet) {
    message("There seems to be no internet connection. Cannot download resources.")
  }
  has_internet
}

.geom_last <- function(data) {
  stopifnot(inherits(data, "sf"))
  sf_col <- attr(data, "sf_column")
  dplyr::relocate(data, !!sf_col,
    .after = dplyr::last_col()
  )
}
