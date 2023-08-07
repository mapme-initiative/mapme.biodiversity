#' Compute specific indicators
#'
#' With \code{calc_indicators()} specific biodiversity indicators
#' can be calculated. A requirement is that the resources that
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
  for (indicator in indicators) x <- .get_single_indicator(x, indicator, ...)
  x
}


#' Calculation of an indicator
#'
#' This functions let's users calculate on or more biodiversity indicators for
#'   a portfolio.
#' @param x A sf object returned by init_portfolio().
#' @param indicator A variable length character vector with the indicators to
#'   calculate.
#' @param ... Additional arguments required by the requested indicators.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr relocate last_col
#' @importFrom tidyr nest
.get_single_indicator <- function(x, indicator, ...) {
  i <- NULL
  # get arguments from function call and portfolio object
  args <- list(...)
  atts <- attributes(x)

  # retrieve the selected indicator
  selected_indicator <- available_indicators(indicator)
  # get processing mode
  processing_mode <- selected_indicator[[indicator]]$processing_mode
  # matching the specified arguments to the required arguments
  params <- .check_resource_arguments(selected_indicator, args)
  # append parameters
  params$verbose <- atts$verbose
  fun <- selected_indicator[[indicator]]$fun
  available_resources <- atts$resources
  required_resources <- selected_indicator[[indicator]]$resources

  if (processing_mode == "asset") {
    p <- progressr::progressor(steps = nrow(x))
    # apply function with parameters and add hidden id column
    results <- furrr::future_map(1:nrow(x), function(i) {
      p()
      resources <- .prep(x[i, ], atts$resources, required_resources)
      .compute(x[i, ], resources, fun, params, i)
    }, .options = furrr::furrr_options(seed = TRUE))
  } else {
    resources <- .prep(x, atts$resources, required_resources)
    results <- .compute(x, resources, fun, params, 1)
  }

  # bind the asset results
  results <- .bind_assets(results)
  # nest the results
  results <- nest(results, !!indicator := !.id)
  # attach results
  x[indicator] <- results[indicator]
  # sent sf column to back and return
  x <- relocate(x, !!attributes(x)[["sf_column"]], .after = last_col())
  x
}



.prep <- function(x, available_resources, required_resources) {
  resources <- purrr::imap(
    required_resources, function(resource_type, resource_name) {
      if (resource_type == "raster") {
        tindex <- read_sf(available_resources[resource_name], quiet = TRUE)
        resource <- .read_raster_source(x, tindex)
      } else if (resource_type == "vector") {
        resource <- .read_vector_source(x, available_resources[[resource_name]])
      } else {
        stop(sprintf("Resource type '%s' currently not supported", resource_type))
      }
      resource
    }
  )

  names(resources) <- names(resources)
  resources
}


.read_vector_source <- function(x, vector_sources) {
  vectors <- purrr::map(vector_sources, function(source) {
    tmp <- read_sf(source, wkt_filter = st_as_text(st_as_sfc(st_bbox(x))))
    st_make_valid(tmp)
  })
  names(vectors) <- basename(vector_sources)
  vectors
}



.read_raster_source <- function(x, tindex) {
  all_bboxes <- lapply(1:nrow(tindex), function(i) paste(as.numeric(st_bbox(tindex[i, ])), collapse = " "))
  is_stacked <- length(unique(unlist(all_bboxes))) == 1

  if (is_stacked) { # current resource/extent all have the same bounding box

    filenames <- basename(tindex$location)
    out <- terra::rast(tindex$location)
    names(out) <- filenames
  } else {
    is_unique <- length(unique(unlist(all_bboxes))) == nrow(tindex)

    if (is_unique) { # all tiles have a different bounding box
      target_files <- tindex$location[unlist(st_intersects(x, tindex))]

      if (length(target_files) == 0) {
        warning("No intersection with resource.")
        return(NULL)
      } else if (length(target_files) == 1) {
        out <- terra::rast(target_files)
      } else {
        # create a vrt for multiple targets
        vrt_name <- tempfile("vrt", fileext = ".vrt")
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
        vrt_name <- tempfile(pattern = sprintf("vrt_%s.vrt", filename))
        tmp <- terra::vrt(target_files, filename = vrt_name)
        names(tmp) <- org_filename
        tmp
      })
      out <- do.call(c, out)
    }
  }

  # crop the source to the extent of the current polygon
  cropped <- try(terra::crop(out, terra::vect(x)))
  if (inherits(cropped, "try-error")) {
    warning(as.character(cropped))
    return(NULL)
  }
  cropped
}


.compute <- function(x, resources, fun, args, i) {
  args <- append(args, resources)
  args$x <- x

  # call the indicator function with the associated parameters
  out <- try(do.call(fun, args = args))

  if (length(out) == 1) {
    if (is.na(out)) {
      out <- list(NA)
    }
  }
  if (inherits(out, "try-error")) {
    warning(sprintf("Error occured at polygon %s with the following error message: %s. \n Returning NAs.", i, out))
    out <- list(NA)
  }
  out # return
}

.bind_assets <- function(results) {
  # bind results to data.frame
  index_tbl <- purrr::map_lgl(results, function(x) inherits(x, c("tbl_df", "data.frame")))

  # case all assets returned tibbles
  if (all(index_tbl)) {
    return(dplyr::bind_rows(results, .id = ".id"))
  }

  # case all assets returned NA
  if (all(!index_tbl)) {
    return(
      tibble::tibble(
        .id = as.character(1:length(results)),
        value = rep(NA, length(results))
      )
    )
  }

  # case some assets returned NA
  if (any(index_tbl) & any(!index_tbl)) {
    colnames <- names(results[[which(index_tbl)[1]]])
    fill_values <- lapply(1:length(colnames), function(x) {
      return(NA)
    })
    fill_values <- tibble::as_tibble(data.frame(fill_values))
    names(fill_values) <- colnames
    for (i in which(!index_tbl)) results[[i]] <- fill_values
    return(tibble::tibble(dplyr::bind_rows(results, .id = ".id")))
  }
}
