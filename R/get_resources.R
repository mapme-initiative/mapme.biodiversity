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
                                 opts = mapme_options()) {
  force(x)
  args <- formals(fun)
  resource_name <- args[["name"]]
  outdir <- .make_path(opts[["outdir"]], resource_name)
  # attach required objects to args list
  args[["x"]] <- x
  args[["outdir"]] <- outdir

  resource_to_add <- try(do.call(fun, args = as.list(args)))

  if (inherits(resource_to_add, "try-error")) {
    msg <- sprintf(paste(
      "Download for resource %s failed. ",
      "Returning unmodified portfolio object.",
      sep = ""
    ), resource_name)
    stop(msg)
  }
  if (is.null(resource_to_add[1])) {
    return(x)
  }
  if (args[["type"]] == "raster") {
    resource_to_add <- .make_footprints(resource_to_add)
  }
  resource_to_add <- list(resource_to_add)
  names(resource_to_add) <- resource_name
  .add_resource(resource_to_add)
  x
}

.make_path <- function(outdir, name) {
  path <- file.path(outdir, name)
  dir.create(path, showWarnings = FALSE)
  path
}

.make_footprints <- function(raster_files) {
  footprints <- lapply(unique(raster_files), function(file) {
    # get BBOX and CRS
    tmp <- rast(file)
    footprint <- st_bbox(tmp) %>% st_as_sfc()
    crs <- crs(tmp)
    # apply precision roundtrip
    footprint <- footprint %>%
      st_sfc(precision = 1e5) %>%
      st_as_binary() %>%
      st_as_sfc()
    # to sf and add location info
    footprint %>%
      st_as_sf(crs = crs) %>%
      dplyr::rename(geom = "x") %>%
      dplyr::mutate(location = file)
  })
  do.call(rbind, footprints)
}
