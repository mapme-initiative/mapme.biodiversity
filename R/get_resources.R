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
  stopifnot(inherits(resource, "sf"))
  stopifnot(type %in% c("raster", "vector"))

  if (is.null(outdir)) {
    resource[["location"]] <- resource[["source"]]
  } else {
    if (verbose) {
      has_progressr <- check_namespace("progressr", error = FALSE)
      if (has_progressr) {
        n <- nrow(resource)
        s <- 1
        if (n > 100) {
          s <- round(n * 0.01)
          n <- 100
        }
        p <- progressr::progressor(n)
      }
    }

    resource[["destination"]] <- file.path(outdir, resource[["filename"]])
    resource <- lapply(1:nrow(resource), function(i) resource[i, ])

    furrr::future_iwalk(resource, function(x, i) {
      attempts <- 0

      while (attempts < retries) {
        attempts <- attempts + 1

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

      if (verbose && has_progressr) {
        if (i %% s == 0) {
          p(message = sprintf("Fetching resource '%s'.", name))
        }
      }
    }, .options = furrr::furrr_options(seed = TRUE, chunk_size = 1L))

    resource <- st_as_sf(do.call(rbind, resource))
    resource[["location"]] <- resource[["destination"]]
    is_available <- sapply(resource[["location"]], spds_exists, what = type)
    resource[is_available, ]
  }
  resource
}
