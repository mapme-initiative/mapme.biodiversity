#' Compute specific indicators
#'
#' With \code{calc_indicators()} specific biodiversity indicators
#' can be calculated. A requirment is that the ressources that
#' are mandatory inputs for the requested indicators are available
#' locally. Multiple indicators and their respective additional
#' arguments can be supplied. You can check available indicators and
#' their requirement via \code{available_indicators()}, but
#' the function will also gracefully inform you about any misspecifications.
#' @param x A biodiversity portfolio object constructed via
#'   \code{init_portfolio()}
#' @param indicators A character vector indicating the requested indicators. All
#'   specified indicators must be supported by the package. You can use
#'   \code{available_indicators()} to get more information, e.g. additional
#'   required arguments and their default values, about the supported indicators
#' @param ... Additional arguments required for the requested indicators. Check
#'  \code{available_indicators()} to learn more about the supported indicators
#'  and their arguments.
#' @keywords function
#' @export
calc_indicators <- function(x, indicators, ...) {
  # check if the requested resource is supported
  required_resources <- .check_requested_indicator(indicators)
  # check if any of the requested resources is already locally available
  existing_resources <- names(attributes(x)$resources)
  .check_existing_resources(
    existing_resources, required_resources,
    needed = TRUE
  )
  ## TODO: check if we can go parallel here. Problem is when errors occur
  # for one resource and it terminates the complete process. We would have
  # to catch that so other processes can terminate successfully.
  for (indicator in indicators) x <- .get_single_indicator(x, indicator, ...)
  x
}


#' Calculation of an indicator
#'
#' This functions let's users calculate on or more biodiversity indicators for
#'   a portfolio.
#' @param x A sf object returned by init_portfolio().
#' @param indicator A variable length charcter vector with the indicators to
#'   calculate.
#' @param ... Additional arguments required by the requested indicators.
#'
#' @keywords internal
#' @importFrom dplyr relocate last_col
#' @importFrom tidyr nest
.get_single_indicator <- function(x, indicator, ...) {
  i <- NULL
  # get arguments from function call and portfolio object
  args <- list(...)
  atts <- attributes(x)
  available_resources <- atts$resources
  tmpdir <- file.path(atts$tmpdir, indicator)
  dir.create(tmpdir, showWarnings = FALSE)
  cores <- atts$cores
  verbose <- atts$verbose

  # retrieve the selected indicator
  selected_indicator <- available_indicators(indicator)
  # match function call
  fun <- match.fun(selected_indicator[[1]]$name)
  # required resources
  required_resources <- selected_indicator[[1]]$inputs
  # matching the specified arguments to the required arguments
  params <- .check_resource_arguments(selected_indicator, args)
  # append parameters
  params$verbose <- atts$verbose
  params$tmpdir <- tmpdir
  params$fun <- fun
  params$available_resources <- available_resources
  params$required_resources <- required_resources

  if (verbose) {
    progressr::handlers(
      progressr::handler_progress(
        format = sprintf(
          " Calculating indicator '%s' [:bar] :percent",
          indicator
        ),
        clear = FALSE,
        width = 60
      )
    )
  }
  # apply function with parameters and add hidden id column
  if (cores > 1) {
    if (verbose) {
      progressr::with_progress({
        params$p <- progressr::progressor(along = 1:nrow(x))
        results <- parallel::mclapply(1:nrow(x), function(i) {
          .prep_and_compute(x[i, ], params, i)
        }, mc.cores = cores)
      })
    } else {
      results <- parallel::mclapply(1:nrow(x), function(i) {
        .prep_and_compute(x[i, ], params, i)
      }, mc.cores = cores)
    }
  } else {
    if (verbose) {
      progressr::with_progress({
        params$p <- progressr::progressor(along = 1:nrow(x))
        results <- lapply(1:nrow(x), function(i) {
          .prep_and_compute(x[i, ], params, i)
        })
      })
    } else {
      results <- lapply(1:nrow(x), function(i) {
        .prep_and_compute(x[i, ], params, i)
      })
    }
  }
  # cleanup the tmpdir for indicator
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  # bind results to data.frame
  results <- do.call(rbind, results)
  # nest the results
  results <- nest(results, !!indicator := !.id)
  # attach results
  x[indicator] <- results[indicator]
  # sent sf column to back and return
  x <- relocate(x, !!attributes(x)[["sf_column"]], .after = last_col())
  x
}


#' Internal indicator routine
#'
#' Helper to abstract preparation and computation
#' of indicators per polygon
#' @keywords internal
.prep_and_compute <- function(shp, params, i) {
  rundir <- file.path(params$tmpdir, i) # create a rundir name
  dir.create(rundir, showWarnings = FALSE) # create the current rundir
  params$rundir <- rundir # change rundir
  params$shp <- shp # enter specific polygon
  # .read_source should return NULL if an error occurs
  processed_resources <- .read_source(params, rundir)
  params <- append(params, processed_resources)
  # call the indicator function with the associated parameters
  out <- do.call(params$fun, args = params)
  if (params$verbose) params$p() # progress tick
  out$.id <- i # add an id variable
  unlink(rundir, recursive = TRUE, force = TRUE) # delete the current rundir
  out # return
}

.read_source <- function(params, rundir) {
  required_resources <- params$required_resources
  available_resources <- params$available_resources
  shp <- params$shp

  processed_resources <- lapply(seq_along(required_resources), function(i) {
    resource_type <- required_resources[[i]]
    resource_name <- names(required_resources)[[i]]
    if (resource_type == "raster") {
      # retrieve tiles that intersect with the shp extent
      tindex <- read_sf(available_resources[resource_name], quiet = TRUE)
      all_bboxes <- lapply(1:nrow(tindex), function(i) paste(as.numeric(st_bbox(tindex[i, ])), collapse = " "))
      is_stacked <- length(unique(unlist(all_bboxes))) == 1
      if (is_stacked) {
        filenames <- gsub(".tif", "", basename(tindex$location))
        out <- terra::rast(tindex$location)
        names(out) <- filenames
      } else {
        target_files <- tindex$location[unlist(st_intersects(shp, tindex))]

        if (length(target_files) == 0) {
          warning("Does not intersect.")
          return(NULL)
        } else if (length(target_files) == 1) {
          out <- terra::rast(target_files)
        } else {
          # create a vrt for multiple targets
          vrt_name <- tempfile("vrt", fileext = ".vrt", tmpdir = rundir)
          out <- terra::vrt(target_files, filename = vrt_name)
        }
      }

      # crop the source to the extent of the current polygon
      out <- tryCatch(
        {
          terra::crop(out, terra::vect(shp), tempdir = rundir)
        },
        error = function(cond) {
          print(cond)
          warning("Cropping failed.", call. = FALSE)
          return(NULL)
        },
        warning = function(cond) {
          print(cond)
          warning("Cropping failed.", call. = FALSE)
          return(NULL)
        }
      )
    }

    if (resource_type == "vector") {
      out <- read_sf(source, wkt_filter = st_as_text(st_geometry(shp)))
    }
    out
  })
  names(processed_resources) <- names(required_resources)
  processed_resources
}
