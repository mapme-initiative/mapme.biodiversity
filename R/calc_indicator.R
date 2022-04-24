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
#' @return The sf portfolio object \code{x} with additional nested list column per
#'   requested indicator.
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
  # Fix for MacOS CI error with duplicated vertices in vector resources
  # Error in s2_geography_from_wkb(x, oriented = oriented, check = check) :
  # Evaluation error: Found 1 feature with invalid spherical geometry.
  # [1] Loop 0 is not valid: Edge 821 is degenerate (duplicate vertex).
  # https://github.com/r-spatial/sf/issues/1762 suggests to deactivate s2,
  # proposition of https://github.com/r-spatial/sf/issues/1902 to dissable
  # s2 and fix the geometry has failed, thus for now falling back to lwgeom
  if (Sys.info()["sysname"] == "Darwin" | grepl("darwin", Sys.info()["sysname"])) {
    s2_org <- sf_use_s2()
    suppressMessages(sf_use_s2(FALSE))
    on.exit(suppressMessages(sf_use_s2(s2_org)))
  }

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
#' @importFrom data.table rbindlist
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
  # get processing mode
  processing_mode <- selected_indicator[[1]]$processing_mode
  # matching the specified arguments to the required arguments
  params <- .check_resource_arguments(selected_indicator, args)
  # append parameters
  params$verbose <- atts$verbose
  params$tmpdir <- tmpdir
  params$fun <- fun
  params$available_resources <- available_resources
  params$required_resources <- required_resources
  params$cores <- cores
  params$processing_mode <- processing_mode
  # set terra temporal directory to rundir
  terra_org <- tempdir()
  dir.create(file.path(tmpdir, "terra"), showWarnings = FALSE)
  terra::terraOptions(tempdir = file.path(tmpdir, "terra"))

  if (processing_mode == "asset") {
    # apply function with parameters and add hidden id column
    results <- pbapply::pblapply(1:nrow(x), function(i) {
      .prep_and_compute(x[i, ], params, i)
    }, cl = cores)
  } else {
    results <- .prep_and_compute(x, params, 1)
  }
  # cleanup the tmpdir for indicator
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  # remove terra tmpdir
  unlink(file.path(tmpdir, "terra"), recursive = TRUE, force = TRUE)
  terra::terraOptions(tempdir = terra_org)
  # bind results to data.frame
  results <- tibble(data.table::rbindlist(results, fill = TRUE, idcol = ".id"))
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
#' @noRd
.prep_and_compute <- function(shp, params, i) {
  rundir <- file.path(params$tmpdir, i) # create a rundir name
  dir.create(rundir, showWarnings = FALSE) # create the current rundir
  params$rundir <- rundir # change rundir
  params$shp <- shp # enter specific polygon
  # .read_source should return NULL if an error occurs
  processed_resources <- .read_source(params, rundir)
  params <- append(params, processed_resources)
  # call the indicator function with the associated parameters
  out <- try(do.call(params$fun, args = params))
  if (length(out) == 1) {
    if (is.na(out)) {
      out <- list(NA)
    }
  }
  if (inherits(out, "try-error")) {
    warning(sprintf("Error occured at polygon %s with the following error message: %s. \n Returning NAs.", i, out))
    out <- list(NA)
  }
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
      tindex <- read_sf(available_resources[resource_name], quiet = TRUE)
      out <- .read_raster_source(shp, tindex, rundir)
    } else if (resource_type == "vector") {
      out <- lapply(available_resources[[resource_name]], function(source) {
        tmp <- read_sf(source, wkt_filter = st_as_text(st_as_sfc(st_bbox(shp))))
        st_make_valid(tmp)
      })
      names(out) <- basename(available_resources[[resource_name]])
    } else {
      stop(sprintf("Resource type '%s' currently not supported", resource_type))
    }
    out
  })
  names(processed_resources) <- names(required_resources)
  processed_resources
}



.read_raster_source <- function(shp, tindex, rundir) {
  all_bboxes <- lapply(1:nrow(tindex), function(i) paste(as.numeric(st_bbox(tindex[i, ])), collapse = " "))
  is_stacked <- length(unique(unlist(all_bboxes))) == 1

  if (is_stacked) { # current resource/extent all have the same bounding box

    filenames <- basename(tindex$location)
    out <- terra::rast(tindex$location)
    names(out) <- filenames
  } else {
    is_unique <- length(unique(unlist(all_bboxes))) == nrow(tindex)

    if (is_unique) { # all tiles have a different bounding box
      target_files <- tindex$location[unlist(st_intersects(shp, tindex))]

      if (length(target_files) == 0) {
        warning("No intersection with resource.")
        return(NULL)
      } else if (length(target_files) == 1) {
        out <- terra::rast(target_files)
      } else {
        # create a vrt for multiple targets
        vrt_name <- tempfile("vrt", fileext = ".vrt", tmpdir = rundir)
        out <- terra::vrt(target_files, filename = vrt_name)
      }
    } else { # some tiles share the same bboxes, and others do not, needs proper merging
      # We assume here that the tiles present in tileindex have a temporal dimension.
      # Thus each timestep should end up in its own layer. Different tiles from
      # the same timestep should be spatially merged. We want to avoid merging
      # different tile from different timesteps. We thus assume some regularity
      # in how the name of a raster file expresses its temporal dimension.
      # With this assumption, we can expect the files in tindex to be ordered.
      # Thus we retrive the index of all files sharing the same bbox and assume
      # that they belong to different timesteps. The files in between these
      # indices thus belong to the previous timestep and we can merge these
      # as a vrt and later join the bands. We always assign the name of the
      # first file as the layername.
      unique_bboxes <- unique(unlist(all_bboxes))
      layer_index <- which(all_bboxes == unique_bboxes[[1]])
      temporal_gap <- layer_index[2] - layer_index[1] - 1
      out <- lapply(layer_index, function(j) {
        target_files <- tindex$location[j:(j + temporal_gap)]
        org_filename <- basename(target_files[1])
        filename <- tools::file_path_sans_ext(org_filename)
        vrt_name <- file.path(rundir, sprintf("vrt_%s.vrt", filename))
        tmp <- terra::vrt(target_files, filename = vrt_name)
        names(tmp) <- org_filename
        tmp
      })
      out <- do.call(c, out)
    }
  }
  # crop the source to the extent of the current polygon
  cropped <- try(terra::crop(out, terra::vect(shp)))
  if (inherits(cropped, "try-error")) {
    warning(as.character(cropped))
    return(NULL)
  }
  cropped
}
