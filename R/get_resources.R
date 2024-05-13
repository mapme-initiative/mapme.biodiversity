.res_defaults <- c("x", "name", "type", "outdir", "verbose", "testing")
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
                                 opts = mapme_options(),
                                 gdal_conf = NULL) {
  force(x)
  args <- formals(fun)
  resource_name <- args[["name"]]
  outdir <- .make_path(opts[["outdir"]], resource_name)
  # attach required objects to args list
  args[["x"]] <- x
  args[["outdir"]] <- outdir

  resource <- try(do.call(fun, args = as.list(args)))
  resource <- .check_footprints(resource, name)

  resource <- .fetch_resource(
    resource = resource,
    name = resource_name,
    type = args[["type"]],
    outdir = outdir,
    gdal_conf = gdal_conf
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
                            outdir = NULL,
                            verbose = TRUE,
                            gdal_conf = NULL) {
  stopifnot(inherits(resource, "sf"))
  stopifnot(type %in% c("raster", "vector"))

  msg <- paste0("Fetching resource ", name, "...")

  if (!is.null(outdir)) {
    resource[["destination"]] <- file.path(outdir, resource[["filename"]])

    params <- list(
      resource[["source"]],
      resource[["destination"]],
      resource[["oo"]],
      resource[["co"]]
    )

    withr::with_envvar(gdal_conf, code = {
      purrr::pwalk(params, function(src, dest, oo, co) {
        .get_spds(
          source = src,
          destination = dest,
          opts = c(oo, co),
          what = type
        )
      },
      .progress = ifelse(verbose, msg, NULL)
      )
    })

    resource[["location"]] <- resource[["destination"]]
    resource
  } else {
    resource[["location"]] <- resource[["source"]]
  }
  resource
}
