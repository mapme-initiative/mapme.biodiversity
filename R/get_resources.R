.res_defaults <- c("x", "name", "type", "outdir", "verbose")
#' Download biodiversity resources
#'
#' `get_resources()` data sets required for the
#' calculation of indicators can be made available. The function supports the
#' specification of several resource functions. To determine the output path,
#' temporary directory and verbosity, the output of `mapme_options()` is used.
#'
#' @param x An `sf` object with features of type `"POLYGON"`
#' @param ... One or more functions for resources/indicators
#' @return `get_resources()` is called for its side effect of making resources
#'  available in the package environment. Returns `x`, invisibly.
#' @name mapme
#' @export
get_resources <- function(x, ...) {
  x <- .check_portfolio(x)
  if (!.has_internet()) {
    return(invisible(x))
  }
  funs <- purrr::map(list(...), function(fun) .check_resource_fun(fun))
  for (fun in funs) .get_single_resource(x, fun)
  invisible(x)
}

.check_resource_fun <- function(fun) {
  if (!inherits(fun, "function")) {
    stop("get_resources() expects you supply one or more functions.")
  }

  args <- formals(fun)
  if (any(!names(args) %in% .res_defaults) || any(!.res_defaults %in% names(args))) {
    msg <- paste(
      "Resource functions are required to have the following default arguments:\n",
      paste(.res_defaults, sep = "", collapse = ", ")
    )
    stop(msg)
  }

  resource_name <- args[["name"]]

  if (resource_name %in% names(.avail_resources())) {
    msg <- sprintf("Resource '%s' is already available.", resource_name)
    message(msg)
  }

  invisible(fun)
}

#' @keywords internal
#' @noRd
.get_single_resource <- function(x = NULL,
                                 fun = NULL,
                                 opts = mapme_options()) {
  force(x)
  args <- formals(fun)
  resource_name <- args[["name"]]
  outdir <- .make_path(opts[["outdir"]], resource_name)
  # attach required objects to args list
  args[["x"]] <- x
  args[["outdir"]] <- outdir

  resource <- try(do.call(fun, args = as.list(args)))
  resource <- .check_footprints(resource, resource_name)

  resource <- .fetch_resource(
    resource = resource,
    name = resource_name,
    type = args[["type"]],
    outdir = outdir,
    verbose = opts[["verbose"]],
    retries = opts[["retries"]]
  )

  resource <- .set_precision(resource)

  resource <- list(resource)
  names(resource) <- resource_name
  .add_resource(resource)

  x
}

.make_path <- function(outdir, name) {
  if (is.null(outdir)) {
    return(outdir)
  }
  path <- file.path(outdir, name)
  dir.create(path, showWarnings = FALSE)
  path
}

.resource_cols <- c("filename", "location", "type", "oo", "co", "source", "geometry")

.check_footprints <- function(resource, name) {
  if (inherits(resource, "try-error")) {
    stop(paste0(
      "Download for resource ", name, " failed.\n",
      "Returning unmodified portfolio."
    ))
  }

  if (!inherits(resource, "sf")) {
    msg <- paste("Resource functions are expected to return sf objects.")
    stop(msg)
  }

  if (any(!names(resource) %in% .resource_cols)) {
    msg <- paste(
      "Resource functions are expected to return sf objects with",
      "columns:", paste(.resource_cols, collapse = ", ")
    )
    stop(msg)
  }

  invisible(resource)
}

.fetch_resource <- function(resource,
                            name = NULL,
                            type = NULL,
                            outdir = mapme_options()[["outdir"]],
                            verbose = mapme_options()[["verbose"]],
                            retries = mapme_options()[["retries"]]) {

  stopifnot(inherits(resource, "sf"), type %in% c("raster", "vector"))

  if (is.null(outdir)) {
    resource[["location"]] <- resource[["source"]]
  } else {
    if (verbose) {
      has_progressr <- check_namespace("progressr", error = FALSE)
      if (has_progressr) {
        n <- nrow(resource)
        s <- 1L
        if (n > 100L) {
          s <- round(n * 0.01)
          n <- 100L
        }
        p <- progressr::progressor(n)
      }
    }

    resource[["destination"]] <- file.path(outdir, resource[["filename"]])
    resource <- lapply(seq_len(nrow(resource)), function(i) resource[i, ])

    furrr::future_iwalk(resource, function(x, i) {
      attempts <- 0L

      while (attempts < retries) {
        attempts <- attempts + 1L

        is_available <- .get_spds(
          source = x[["source"]],
          destination = x[["destination"]],
          opts = unlist(c(x[["co"]], x[["oo"]])),
          what = x[["type"]]
        )

        if (is_available) {
          attempts <- retries
        }
      }

      if (verbose && has_progressr && (i %% s == 0L)) {
        p(message = sprintf("Fetching resource '%s'.", name))
      }
    }, .options = furrr::furrr_options(seed = TRUE, chunk_size = 1L))

    resource <- sf::st_as_sf(do.call(rbind, resource))
    resource[["location"]] <- resource[["destination"]]
    is_available <- sapply(resource[["location"]], spds_exists, what = type)
    if (isFALSE(all(is_available))) {
      not_available_resource <- resource[!is_available, ]
      msg <- sprintf("%d out of %d resources are not available for %s",
                     nrow(not_available_resource), nrow(resource), name)
      log_dir <- mapme_options()[["log_dir"]]
      if (!is.null(log_dir)) {
        dsn <- file.path(log_dir, paste0(Sys.Date(), "_", name,
                                         "_mapme-error-resources.gpkg"))
        # drop list columns to write GPKG
        not_available_resource <- not_available_resource[,
                                  setdiff(names(not_available_resource), c("oo", "co"))]
        # sf::st_write(subset(not_available_resource, select = -c(oo, co)),
        #              dsn, append = TRUE, quiet = TRUE)
        sf::st_write(not_available_resource, dsn, append = TRUE, quiet = TRUE)
        msg <- paste(msg, sprintf("The list can be found in %s", dsn), sep = "\n")
      }
      warning(msg, call. = FALSE)
      resource <- resource[is_available, ]
    }
  }
  return(resource)
}

