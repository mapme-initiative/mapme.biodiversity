.ind_defaults <- c("x", "name", "mode", "verbose")
#' Compute specific indicators
#'
#' `calc_indicators()` calculates specific biodiversity indicators. A
#' requirement is that the resources that are mandatory inputs for the requested
#' indicators are available locally. Multiple indicators and their respective
#' additional arguments can be supplied.
#'
#' @param  x An `sf` object with features of type `"POLYGON"`
#' @param ... One or more functions for resources/indicators
#' @return `calc_indicators()` returns `x`, invisibly, with an additional nested
#'   list column per requested indicator.
#' @name mapme
#' @export
#' @include get_resources.R
calc_indicators <- function(x, ...) {
  x <- .check_portfolio(x)
  funs <- list(...)
  funs <- purrr::map(funs, function(fun) .check_indicator_fun(fun))
  req_resources <- purrr::map(funs, function(fun) .get_req_resources(fun))
  .check_avail_resources(names(.avail_resources()), unique(unlist(req_resources)))
  for (fun in funs) x <- .get_single_indicator(x, fun)
  x
}

.check_indicator_fun <- function(fun) {
  if (!inherits(fun, "function")) {
    stop("calc_indicators() expects you to supply one or more indicator functions.")
  }
  args <- names(formals(fun))
  if (any(!.ind_defaults %in% args)) {
    msg <- paste(
      "Indicator functions are required to have the following default arguments:\n",
      paste(.ind_defaults, sep = "", collapse = ", ")
    )
    stop(msg)
  }
  invisible(fun)
}

.get_req_resources <- function(fun) {
  if (!inherits(fun, "function")) {
    stop("calc_indicators() expects you to supply one or more indicator functions.")
  }
  args <- names(formals(fun))
  req_resources <- setdiff(args, .ind_defaults)
  if (length(req_resources) == 0) {
    stop("The indicator functions lacks required resources.")
  }
  req_resources
}

.check_avail_resources <- function(avail_resources, req_resources) {
  if (any(!req_resources %in% avail_resources)) {
    not_avail <- req_resources[which(!req_resources %in% avail_resources)]
    msg <- sprintf(
      "The following requested %s not available: %s.",
      ifelse(length(not_avail) == 1, "resource is", "resources are"),
      paste(not_avail, collapse = ", ")
    )
    stop(msg)
  }
}

.get_single_indicator <- function(x = NULL,
                                  fun = NULL,
                                  opts = mapme_options()) {
  args <- formals(fun)
  indicator_name <- args[["name"]]
  processor <- switch(args[["mode"]],
    asset = .asset_processor,
    portfolio = .portfolio_processor,
    stop("Processing mode not supported.")
  )
  results <- processor(
    x, fun, .avail_resources(), .get_req_resources(fun),
    opts[["verbose"]]
  )
  x <- .merge_results(x, results, indicator_name)
  x
}

.asset_processor <- function(x,
                             fun,
                             avail_resources,
                             req_resources,
                             verbose) {
  p <- progressr::progressor(steps = nrow(x))
  furrr::future_map(1:nrow(x), function(i) {
    p()
    resources <- .prep_resources(x[i, ], avail_resources, req_resources)
    result <- .compute(x[i, ], resources, fun, verbose)
    .check_single_asset(result, i)
  }, .options = furrr::furrr_options(seed = TRUE))
}

.portfolio_processor <- function(x,
                                 fun,
                                 avail_resources,
                                 req_resources,
                                 verbose) {
  resources <- .prep_resources(x, avail_resources, req_resources)
  results <- .compute(x, resources, fun, verbose)
  if (!inherits(results, "list")) {
    stop("Expected output for processing mode 'portfolio' is a list.")
  }
  results <- purrr::imap(results, function(r, i) .check_single_asset(r, i))
  results
}

.prep_resources <- function(x, avail_resources, req_resources) {
  if (any(!req_resources %in% names(avail_resources))) {
    stop("Some required resources are not available.")
  }
  out <- purrr::map(req_resources, function(resource_name) {
    resource <- avail_resources[[resource_name]]
    resource_type <- ifelse(inherits(resource, "sf"), "raster", "vector")
    reader <- switch(resource_type,
      raster = .read_raster,
      vector = .read_vector,
      stop(sprintf("Resource type '%s' currently not supported", resource_type))
    )
    reader(x, resource)
  })
  names(out) <- req_resources
  out
}

.read_vector <- function(x, vector_sources) {
  vectors <- purrr::map(vector_sources, function(source) {
    read_sf(source, wkt_filter = st_as_text(st_as_sfc(st_bbox(x))))
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

  out <- lapply(1:n_timesteps, function(i) {
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

.compute <- function(x, resources, fun, verbose) {
  args <- list(verbose = verbose)
  args <- append(args, resources)
  args[["x"]] <- x
  try(do.call(what = fun, args = args), silent = TRUE)
}

.check_single_asset <- function(obj, i) {
  if (inherits(obj, "try-error")) {
    warning(sprintf("At asset %s an error occured. Returning NA.\n", i), obj)
    return(NA)
  }

  if (!inherits(obj, "tbl_df")) {
    warning(sprintf("At asset %s a non-tibble object was returned. Returning NA.\n", i), obj)
    return(NA)
  }

  if (nrow(obj) == 0) {
    warning(sprintf("At asset %s a 0-length tibble was returned. Returning NA.", i))
    return(NA)
  }
  obj
}

.merge_results <- function(x, results, name) {
  results <- .bind_assets(results)
  results <- tidyr::nest(results, !!name := !.id)
  if (name %in% names(x)) {
    warning(sprintf(
      "Indicator column '%s' is already present. Overwriting now.",
      name
    ))
  }
  x[name] <- results[name]
  dplyr::relocate(x, !!attributes(x)[["sf_column"]], .after = dplyr::last_col())
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
