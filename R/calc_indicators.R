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
  req_resources <- .check_requested_indicator(indicators)
  # check if any of the requested resources is already locally available
  existing_resources <- names(attributes(x)[["resources"]])
  .check_existing_resources(
    existing_resources, req_resources,
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
  processing_mode <- selected_indicator[[indicator]][["processing_mode"]]
  # matching the specified arguments to the required arguments
  params <- .check_resource_arguments(selected_indicator, args)
  # append parameters
  params[["verbose"]] <- atts[["verbose"]]
  fun <- selected_indicator[[indicator]][["fun"]]
  avail_resources <- atts[["resources"]]
  req_resources <- selected_indicator[[indicator]][["resources"]]

  processor <- switch(processing_mode,
                      asset = .asset_processor,
                      portfolio = .portfolio_processor,
                      stop(sprintf("Processing mode '%s' is not supported.", processing_mode)))

  results <- processor(x, fun, avail_resources, req_resources, params)
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



.prep_resources <- function(x, avail_resources, req_resources) {
  if (any(!names(req_resources) %in% names(avail_resources))) {
    stop("Some required resources are not available.")
  }
  purrr::imap(req_resources, function(resource_type, resource_name) {
    reader <- switch(resource_type,
                     raster = .read_raster,
                     vector = .read_vector,
                     stop(sprintf("Resource type '%s' currently not supported", resource_type)))
    reader(x, avail_resources[[resource_name]])})
}

.read_vector <- function(x, vector_sources) {
  vectors <- purrr::map(vector_sources, function(source) {
    tmp <- read_sf(source, wkt_filter = st_as_text(st_as_sfc(st_bbox(x))))
    st_make_valid(tmp)
  })
  names(vectors) <- basename(vector_sources)
  vectors
}

.read_raster <- function(x, tindex) {

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
  cropped <- try(terra::crop(out, terra::vect(x), snap = "out"))
  if (inherits(cropped, "try-error")) {
    warning(as.character(cropped))
    return(NULL)
  }
  cropped
}

.asset_processor <- function(
    x,
    fun,
    avail_resources,
    req_resources,
    params){

  p <- progressr::progressor(steps = nrow(x))
  furrr::future_map(1:nrow(x), function(i) {
    p()
    resources <- .prep_resources(x[i, ], avail_resources, req_resources)
    result <- .compute(x[i, ], resources, fun, params)
    .check_single_asset(result, i)
  }, .options = furrr::furrr_options(seed = TRUE))
}

#' @noRd
#' @importFrom utils str
.check_single_asset <- function(obj, i){

  if (inherits(obj, "try-error")) {
    warning(sprintf("At asset %s an error occured. Returning NA.\n", i), obj)
    return(NA)
  }

  if (!inherits(obj, "tbl_df")) {
    warning(sprintf("At asset %s a non-tibble object was returned. Returning NA.\n", i), str(obj))
    return(NA)
  }

  if (nrow(obj) == 0) {
    warning(sprintf("At asset %s a 0-length tibble was returned. Returning NA.", i))
    return(NA)
  }
  obj
}

.portfolio_processor <- function(
    x,
    fun,
    avail_resources,
    req_resources,
    params ){

  resources <- .prep_resources(x, avail_resources, req_resources)
  results <- .compute(x, resources, fun, params)
  if (!inherits(results, "list")) {
    stop("Expected output for processing mode 'portfolio' is a list.")
  }
  results <- purrr::imap(results, function(r, i) .check_single_asset(r, i))
  results
}

.compute <- function(x, resources, fun, args) {
  args <- append(args, resources)
  args[["x"]] <- x
  try(do.call(what = fun, args = args), silent = TRUE)
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
