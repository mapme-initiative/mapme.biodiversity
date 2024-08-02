.ind_defaults <- c("x", "name", "mode", "aggregation", "verbose")
.ind_cols <- c("datetime", "variable", "unit", "value")
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
    msg <- "calc_indicators() expects you to supply one or more indicator functions."
    stop(msg)
  }
  args <- names(formals(fun))
  if (any(!.ind_defaults %in% args)) {
    msg <- "Indicator functions are required to have the following default arguments:\n"
    msg <- paste(msg, paste(.ind_defaults, sep = "", collapse = ", "))
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
    msg <- "The following requested %s not available: %s."
    msg <- sprintf(
      msg,
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
    x = x,
    fun = fun,
    avail_resources = .avail_resources(),
    req_resources = .get_req_resources(fun),
    chunk_size = opts[["chunk_size"]],
    aggregation = args[["aggregation"]],
    verbose = opts[["verbose"]]
  )

  x <- .add_indicator_column(x, results, indicator_name)
  x
}

.asset_processor <- function(x,
                             fun,
                             avail_resources,
                             req_resources,
                             chunk_size,
                             aggregation,
                             verbose) {
  assetid <- NULL
  x_chunk <- .chunk(x, chunk_size)

  n <- nrow(x_chunk)
  s <- 1
  if (n > 100) {
    s <- round(n * 0.01)
    n <- 100
  }

  if (verbose) {
    has_progressr <- check_namespace("progressr", error = FALSE)
    if (has_progressr) {
      p <- progressr::progressor(n)
    }
  }

  results <- furrr::future_map(1:nrow(x_chunk), function(i) {
    chunk <- x_chunk[i, ]
    resources <- prep_resources(chunk, avail_resources, req_resources, mode = "asset")
    result <- .compute(chunk, resources, fun, verbose)

    if (verbose && has_progressr) {
      if (i %% s == 0) {
        p()
      }
    }

    .check_single_asset(result, chunk)
  }, .options = furrr::furrr_options(seed = TRUE, chunk_size = s))

  results <- split(results, x_chunk[["assetid"]])
  results <- purrr::map(results, .combine_chunks, aggregation)
  results <- results[as.character(x[["assetid"]])]
  unname(results)
}

.portfolio_processor <- function(x,
                                 fun,
                                 avail_resources,
                                 req_resources,
                                 chunk_size,
                                 aggregation,
                                 verbose) {
  x_bbox <- st_as_sf(st_as_sfc(st_bbox(x)))
  resources <- prep_resources(x_bbox, avail_resources, req_resources, mode = "portfolio")
  results <- .compute(x, resources, fun, verbose)
  if (!inherits(results, "list")) {
    stop("Expected output for processing mode 'portfolio' is a list.")
  }
  results <- purrr::map(1:length(results), function(i) {
    .check_single_asset(results[[i]], x[i, ])
  })
  results
}

.process <- function(x, fun, avail_resources, req_resources, verbose) {
  resources <- prep_resources(x, avail_resources, req_resources)
  .compute(x, resources, fun, verbose)
}

.compute <- function(x, resources, fun, verbose) {
  args <- list(verbose = verbose)
  args <- append(args, resources)
  args[["x"]] <- x
  try(do.call(what = fun, args = args), silent = TRUE)
}

.check_single_asset <- function(obj,
                                asset = NULL,
                                log_dir = mapme_options()[["log_dir"]]) {
  obj_names <- names(obj)
  if (!inherits(obj, "tbl_df") || nrow(obj) == 0 || !identical(obj_names, .ind_cols)) {
    warning(obj)
    if (!is.null(log_dir) && !is.null(asset)) {
      dsn <- file.path(log_dir, paste0(Sys.Date(), "_mapme-error-assets.gpkg"))
      st_write(asset, dsn, append = TRUE, quiet = TRUE)
    }
    return(NULL)
  }
  obj
}

.add_indicator_column <- function(x, results, name) {
  if (name %in% names(x)) {
    msg <- "Indicator column '%s' is already present. Overwriting now."
    warning(sprintf(msg, name))
  }
  x[name] <- list(results)
  .geom_last(x)
}
