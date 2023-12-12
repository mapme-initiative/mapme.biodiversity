#' Download specific biodiversity resources
#'
#' With \code{get_resources()} specific biodiversity data sets required for the
#' calculation of indicators can be downloaded. The function supports the
#' specification of several indicators and their respective additional
#' arguments. You can check the required arguments via
#' \code{available_resources()}, but the function will gracefully inform you
#' about any misspecifications.
#' @param x A biodiversity portfolio object constructed via
#'   \code{init_portfolio()}
#' @param resources A character vector indicating the requested resources. All
#'   specified resources must be supported by the package. You can use
#'   \code{available_resources()} to get more information, e.g. additional
#'   required arguments and their default values, about the supported resources.
#' @param ... Additional arguments required for the requested resources. Check
#'  \code{available_resources()} to learn more about the supported resources and
#'  their arguments.
#' @return Primarily called for the side effect of downloading resources. Returns
#'   the sf portfolio object \code{x} with its attributes amended by the requested resources.
#' @keywords function
#' @export
get_resources <- function(x, resources, ...) {
  connection_available <- curl::has_internet()
  if (!connection_available) {
    stop("There seems to be no internet connection. Cannot download resources.")
  }
  # check if the requested resource is supported
  .check_requested_resources(resources)
  # check if any of the requested resources is already locally available
  existing_resources <- attributes(x)[["resources"]]
  resources <- .check_existing_resources(names(existing_resources), resources)

  if (length(resources) == 0) {
    return(x)
  }

  # get the resources
  ## TODO: check if we can go parallel here. Problem is when errors occur
  # for one resource and it terminates the complete process. We would have
  # to catch that so other processes can terminate successfully.
  for (resource in resources) x <- .get_single_resource(x, resource, ...)
  x
}


#' Internal function used to process a single resource
#'
#' This function is called internally by \code{get_resources()} to process
#' a single resource. It does argument matching filling any argument not
#' specified by the user with its default value from the backlog functions. It
#' informs users about the resource, the argument and the default value. It does
#' not check for the correctness of user specified arguments (this is handled
#' within the respective resource function). In the case the resource function
#' issues a warning or error it fails gracefully informing the user but
#' continuing processing other requested resources.
#'
#' @param x A portfolio object
#' @param resource A character vector of length one indicating a supported
#'   resource
#' @param ... Any additional arguments. The relevant arguments for the indicator
#'   function are matched. If there are missing arguments, the default value is
#'   used.
#' @keywords internal
#' @noRd
.get_single_resource <- function(x, resource, ...) {
  args <- list(...)
  atts <- attributes(x)

  rundir <- file.path(atts[["outdir"]], resource)
  dir.create(rundir, showWarnings = FALSE)

  selected_resource <- available_resources(resource)
  # match function
  fun <- selected_resource[[resource]][["fun"]]
  # matching the specified arguments to the required arguments
  params <- .check_resource_arguments(selected_resource, args)
  params[["x"]] <- x
  params[["rundir"]] <- rundir
  params[["verbose"]] <- atts[["verbose"]]

  # conduct download function, TODO: we can think of an efficient way for
  # parallel downloads here or further upstream
  # if files to not exist use download function to download to tmpdir
  if (atts[["verbose"]]) {
    message(sprintf("Starting process to download resource '%s'........", resource))
  }

  resource_to_add <- try(do.call(fun, args = params))
  if (inherits(resource_to_add, "try-error")) {
    stop(sprintf(
      paste("Download for resource %s failed. ",
        "Returning unmodified portfolio object.",
        sep = ""
      ),
      resource
    ))
  }

  if (attr(x, "testing")) {
    return(resource_to_add)
  }

  # we included an error checker so that we can still return a valid object
  # even in cases that one or more downloads fail
  if (is.na(resource_to_add[1])) {
    return(x)
  }

  # if the selected resource is a raster resource create tileindex
  if (selected_resource[[resource]]$type == "raster") {
    resource_to_add <- .make_footprints(resource_to_add)
  }

  # add the new resource to the attributes of the portfolio object
  resource_to_add <- list(resource_to_add)
  names(resource_to_add) <- resource
  atts[["resources"]] <- append(atts[["resources"]], resource_to_add)
  attributes(x) <- atts
  x
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
