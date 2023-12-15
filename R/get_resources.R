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
#' @param download Logical, indicating if resource files should be fetched
#'   from the source location and written to the output directory.
#' @param ... Additional arguments required for the requested resources. Check
#'  \code{available_resources()} to learn more about the supported resources and
#'  their arguments.
#' @return Primarily called for the side effect of downloading resources. Returns
#'   the sf portfolio object \code{x} with its attributes amended by the requested resources.
#' @keywords function
#' @export
get_resources <- function(x, resources, download = FALSE, ...) {
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
  for (resource in resources) x <- .get_single_resource(x, resource, download, ...)
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
#' @param download Logical, indicating if resource files should be fetched
#'   from the source location and written to the output directory.
#' @param ... Any additional arguments. The relevant arguments for the indicator
#'   function are matched. If there are missing arguments, the default value is
#'   used.
#' @keywords internal
#' @noRd
.get_single_resource <- function(x, resource, download = FALSE, ...) {
  args <- list(...)
  atts <- attributes(x)

  rundir <- tempfile()
  dir.create(rundir)
  # TODO: we have to evaluate how to create directories on remote locations

  if(!is.null(atts[["outdir"]])){
    outdir <- file.path(atts[["outdir"]], resource)
    dir.create(outdir, showWarnings = FALSE)
  } else {
    outdir <- NULL
  }

  selected_resource <- available_resources(resource)
  type <- selected_resource[[resource]][["type"]]
  # match function
  fun <- selected_resource[[resource]][["fun"]]
  # matching the specified arguments to the required arguments
  params <- .check_resource_arguments(selected_resource, args)
  params[["x"]] <- x
  params[["rundir"]] <- rundir
  params[["outdir"]] <- outdir
  params[["verbose"]] <- atts[["verbose"]]
  gdal_config_global <- get_mapme_gdal_config()
  gdal_config_resource <- args[["gdal_config"]]

  # conduct download function, TODO: we can think of an efficient way for
  # parallel downloads here or further upstream
  # if files to not exist use download function to download to tmpdir
  if (atts[["verbose"]]) {
    message(sprintf("Starting process to download resource '%s'........", resource))
  }

  resource_to_add <- .call_resource_fun(fun, params, resource, gdal_config_resource)

  if (attr(x, "testing")) {
    return(resource_to_add)
  }

  resource_to_add <- .fetch_resource(
    resource = resource_to_add,
    name = resource,
    outdir = outdir,
    type = type,
    verbose = atts[["verbose"]],
    gdal_config_global = gdal_config_global,
    gdal_config_resource = gdal_config_resource)

  resource_to_add <- .set_precision(resource_to_add, precision = 1e5)

  # add the new resource to the attributes of the portfolio object
  resource_to_add <- list(resource_to_add)
  names(resource_to_add) <- resource
  atts[["resources"]] <- append(atts[["resources"]], resource_to_add)
  attributes(x) <- atts
  x
}


.call_resource_fun <- function(fun, args, name, gdal_config = list()){

  withr::with_envvar(gdal_config, code = {
    resource <- try(do.call(fun, args = args))
  })

  if (inherits(resource, "try-error")) {
    stop(paste0("Download for resource ", name, " failed.\n",
                "Returning unmodified portfolio."))
  }

  if (!inherits(resource, "sf")) {
    stop("resource functions are expected to return sf objects with data footprints.")
  }

  if (any(!names(resource) %in% c("source", "filename", "opts", "geometry"))){
    stop("resource functions are expected to return sf object with columns 'source', 'filename', 'opts', and 'geometry'.")
  }
  resource
}


.fetch_resource <- function(
    resource,
    name = NULL,
    type = NULL,
    outdir = NULL,
    verbose = TRUE,
    gdal_config_global = get_mapme_gdal_config(),
    gdal_config_resource = NULL) {

  name <- paste0("Fetching resource ", name, "...")
  if (!is.null(outdir)){
    resource[["destination"]] <- file.path(outdir, resource[["filename"]])
    suceeded <- purrr::pwalk(list(resource[["source"]], resource[["destination"]], resource[["opts"]]),
      function(src, dest, opt) {
        .get_spds(src=src, dest=dest, opts=opt, what=type,gdal_config_global,gdal_config_resource)
      },
      .progress = ifelse(verbose, name, NULL))
    resource[["location"]] <- resource[["destination"]]
    attributes(resource)[["gdal_config"]]<- gdal_config_global
  } else {
    resource[["location"]] <- resource[["source"]]
    attributes(resource)[["gdal_config"]]<- gdal_config_resource
  }
  resource[c("location", "opts")]
}



