#' Download specific biodiversity resources
#'
#' With \code{get_resources()} specific biodiversity data sets required for the
#' calculation of indicators can be downloaded. The function supports the specification
#' of several indicators and their respective additional arguments. You can
#' check the required arguments via \code{available_resources()}, but
#' the function will gracefully inform you about any misspecifications.
#' @param x A biodiversity portfolio object constructed via \code{init_portfolio()}
#' @param resources A character vector indicating the requested resources. All
#'   specified resources must be supported by the package. You can use \code{available_resources()}
#'   to get more information, e.g. additional required arguments and their default
#'   values, about the supported resources.
#' @param ... Additional arguments required for the requested resources. Check
#'  \code{available_resources()} to learn more about the supported resources and
#'  their arguments.
#' @export
get_resources <- function(x, resources, ...){
  # check if the requested resource is supported
  .check_requested_resources(resources)
  # check if any of the requested resources is already locally available
  existing_resources = attributes(x)$resources
  resources = .check_existing_resources(names(existing_resources), resources)
  if(length(resources) == 0) return(x)
  # get the resources
  ## TODO: check if we can go parallel here. Problem is when errors occur
  # for one resource and it terminates the complete process. We would have
  # to catch that so other processes can terminate successfully.
  for(resource in resources) x = .get_single_resource(x, resource, ...)
  x
}


#' Internal function used to process a single resource
#'
#' This function is called internally by \code{get_resources()} to process
#' a single resource. It does argument matching filling any argument not specified
#' by the user with its default value from the backlog functions. It informs
#' users about the resource, the argument and the default value. It does not
#' check for the correctness of user specified arguments (this is handled within
#' the respective resource function). In the case the resource function issues
#' a warning or error it fails gracefully informing the user but continuing
#' processing other requested resources.
#'
#' @param x A portfolio object
#' @param resource A character vector of length one indicating a supported resource
#' @param ... Any additional arguments. The relevant arguments for the indicator
#'   function are matched. If there are missing arguments, the default value is used.
#' @keywords internal
.get_single_resource <- function(x, resource, ...){

  args = list(...)
  atts = attributes(x)
  outdir = atts$outdir
  tmpdir = atts$tmpdir
  selected_resource = available_resources(resource)
  # match function
  fun = match.fun(selected_resource[[1]]$downloader)
  # matching the specified arguments to the required arguments
  relevant_args = .check_resource_arguments(selected_resource, args)
  # conduct download function, TODO: we can think of an efficient way for parallel downloads here or further upstream
  filename = ifelse(selected_resource[[1]]$type == "raster",
                    file.path(outdir, paste0(resource, ".tif")),
                    file.path(outdir, paste0(resource, ".gpkg"))
  )

  if (file.exists(filename)) {
    message(sprintf("Resource '%s' exists in output directory. Remove if you wish to re-download", filename))

  } else { # if files to not exist use download function to download to tmpdir

    downloaded_files = tryCatch({
      fun(st_bbox(x), relevant_args, tmpdir = tmpdir)
    },
    error = function(e){
      e
    },
    warning = function(e){
      e
    })

    # we included an error checker so that we can still return a valid object
    # even in cases that one or more downloads fail
    if(inherits(downloaded_files, c("error", "warning"))){
      warning(sprintf("Download for resource %s failed. Returning unmodified portfolio object.", resource))
      return(x)
    }

    # we translate rasters to a single COG and vectors to a GPKG
    if(tools::file_ext(filename) == "tif"){
      .tiffs2COGs(downloaded_files, filename, tmpdir)
    } else {
      .vec2GPKG(downloaded_files, filename, tmpdir)
    }
  }

  # add the new resource to the attributes of the portfolio object
  if(is.na(atts$resources)){
    atts$resources = filename
    names(atts$resources) = resource
  }
  atts$resources[resource] = filename
  attributes(x) = atts
  x
}
