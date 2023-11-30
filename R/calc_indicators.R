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

  if (st_crs(x) != st_crs(tindex)) {
    x <- st_transform(x, st_crs(tindex))
  }

  geoms <- tindex[["geom"]]
  unique_geoms <- unique(geoms)
  grouped_geoms <- match(geoms, unique_geoms)
  names(grouped_geoms) <- tindex[["location"]]
  grouped_geoms <- sort(grouped_geoms)

  n_tiles <- length(unique(grouped_geoms))
  n_timesteps <- unique(table(grouped_geoms))

  if (length(n_timesteps) > 1) {
    stop("Did not find equal number of tiles per timestep.")
  }

  out <- lapply(1:n_timesteps, function(i){
    index <- rep(FALSE, n_timesteps)
    index[i] <- TRUE
    filenames <- names(grouped_geoms[index])
    layer_name <- tools::file_path_sans_ext(basename(filenames[1]))
    vrt_name <- tempfile(pattern = sprintf("vrt_%s", layer_name), fileext = ".vrt")
    tmp <- terra::vrt(filenames, filename = vrt_name)
    names(tmp) <- layer_name
    tmp
  })
  out <- do.call(c, out)

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

  if (inherits(out, "try-error")) {
    warning(sprintf("Error occured at polygon %s with the following error message: %s. \n Returning NAs.", i, out))
    out <- list(NA)
  }

  if (!inherits(out, "tbl_df")){
    warning(sprintf("At polygon %s a non-tibble object was returned: %s \n Returning NAs.", i, out))
    out <- list(NA)
  }

  out # return
}

.bind_assets <- function(results) {
  # bind results to data.frame
  index_tbl <- purrr::map_lgl(results, function(x) inherits(x, c("tbl_df", "data.frame")))
  # check for 0 length tibbles
  n_rows <- sapply(results[index_tbl], nrow)
  if (any(n_rows == 0)) {
    stop(paste("0-length tibbles returned for some assets.\n",
               "Make sure the indicator function returns NA if it cannot be calculated for an asset."))
  }

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
