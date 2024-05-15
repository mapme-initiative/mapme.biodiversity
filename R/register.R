.pkgenv <- new.env(parent = emptyenv())

.pkgenv$resources <- list()
.pkgenv$indicators <- list()
.pkgenv$avail_resources <- list()

.onLoad <- function(libname, pkgname) {
  .pkgenv$outdir <- tempfile()
  dir.create(.pkgenv$outdir, showWarnings = FALSE)
  .pkgenv$log_dir <- NULL
  .pkgenv$verbose <- TRUE
  .pkgenv$aria_bin <- NULL
  .pkgenv$testing <- FALSE
  .pkgenv$chunk_size <- 100000
  invisible()
}

#' Portfolio methods for mapme.biodiversity
#'
#' `mapme_options()` sets default options for mapme.biodiversity to control the
#' behavior of downstream functions.
#' Mainly, the output path as well as the temporal directory for intermediate
#' files can be set. Additionally, the verbosity can be set. The testing options
#' should not be set by users, as it controls the behavior of the package during
#' automated test pipelines. Might be extended by other options in the future.
#'
#' @param ... ignored
#' @param outdir A length one character indicating the output path.
#' @param chunk_size A numeric of length one giving the maximum chunk area in ha.
#'   Defaults to 100,000 ha.
#' @param aria_bin A character vector to an aria2c executable for parallel
#'  downloads.
#' @param verbose A logical, indicating if informative messages should be printed.
#' @param testing A logical. Not to be set by users. Controls the behavior
#'   during automated test pipelines.
#' @param log_dir A character path pointing toward a GDAL-writable destination
#'   used to log erroneous assets. Defaults to NULL, meaning that erroneous
#'   assets will not be serialized to disk. If specified, a GPKG named
#'   `file.path(log_dir, paste0(Sys.Date(), "_mapme-error-assets.gpkg"))` will
#'   be created and appended to in case of erroneous assets.
#' @return `mapme_options()` returns a list of options if no arguments are specified. Otherwise sets
#'   matching arguments to new values in the package's internal environment.
#' @name mapme
#' @export
#'
#' @examples
#' library(mapme.biodiversity)
#' mapme_options()
mapme_options <- function(..., outdir, chunk_size, verbose, aria_bin, testing, log_dir) {
  if (!missing(outdir)) {
    stopifnot(is.character(outdir) && length(outdir) == 1)
    if (!dir.exists(outdir)) {
      stop("outdir must point to an existing directory")
    }
    .pkgenv$outdir <- outdir
  }

  if (!missing(chunk_size)) {
    stopifnot(length(chunk_size) == 1 && is.numeric(chunk_size))
    .pkgenv$chunk_size <- chunk_size
  }

  if (!missing(verbose)) {
    stopifnot(is.logical(verbose))
    .pkgenv$verbose <- verbose
  }

  if (!missing(aria_bin)) {
    .pkgenv$aria_bin <- .check_aria2(aria_bin)
  }

  if (!missing(testing)) {
    stopifnot(is.logical(testing))
    .pkgenv$testing <- testing
  }

  if (!missing(log_dir)){
    stopifnot(is.null(log_dir) | (is.character(log_dir) && length(log_dir) == 1))
    .pkgenv$log_dir <- log_dir
  }

  if (nargs() == 0) {
    return(list(
      outdir = .pkgenv$outdir,
      chunk_size = .pkgenv$chunk_size,
      verbose = .pkgenv$verbose,
      aria_bin = .pkgenv$aria_bin,
      testing = .pkgenv$testing,
      log_dir = .pkgenv$log_dir
    ))
  }
}

.check_aria2 <- function(aria_bin) {
  aria_output <- try(system2(aria_bin, args = "--version", stdout = TRUE, stderr = FALSE), silent = TRUE)
  if (inherits(aria_output, "try-error") | !grepl("aria2 version", aria_output[1])) {
    warning(paste(
      "Argument 'aria_bin' does not point to a executable aria2 installation.",
      "The package will use R internal download utility."
    ))
    aria_bin <- NULL
  }
  return(aria_bin)
}


.check_char <- function(obj, name) {
  if (!inherits(obj, "character") || length(obj) > 1 || nchar(obj) == 0) {
    stop(paste0(name, " needs to be a single charachter string"))
  }
}
#' Register or list resources in mapme.biodiversity
#'
#' `register_resource()` is used to register a new resource function with base
#' information to the package's internal environment used to inform users about
#' available resources. Note, registering a custom resource will
#' only have effect for the current R session.
#'
#' @param name A character vector indicating the name of the resource.
#' @param description A character vector with a basic description
#' @param licence A character vector indicating the licence of the resource.
#'   In case it is a custom licence, put a link to the licence text.
#' @param source Optional, preferably a URL where the data is found.
#' @param type A character vector indicating the type of the resource. Either
#'   'vector' or 'raster'.
#' @param source Optional, preferably a URL where the data is found.
#'
#' @return `register_resource()` is called for the side-effect of registering a resource.
#' @name resources
#' @export
#'
#' @examples
#' \dontrun{
#' register_resource(
#'   name = "gfw_treecover",
#'   description = "Global Forest Watch - Percentage of canopy closure in 2000",
#'   licence = "CC-BY 4.0",
#'   source = "https://data.globalforestwatch.org/documents/tree-cover-2000/explore",
#'   type = "raster"
#' )
#' }
register_resource <- function(name = NULL,
                              description = NULL,
                              licence = NULL,
                              source = NULL,
                              type = NULL) {
  if (any(is.null(name), is.null(description), is.null(licence), is.null(source), is.null(type))) {
    stop("neither name, description, licence, source, nor type can be NULL")
  }

  .check_char(name, "name")
  .check_char(description, "description")
  .check_char(licence, "licence")
  .check_char(source, "source")
  .check_char(type)

  if (name %in% names(.pkgenv$resources)) {
    warning(paste("resource with name", name, "already registered"))
  }

  if (!type %in% c("vector", "raster")) {
    stop("type needs to be one of 'vector' or 'raster'")
  }

  resource <- tibble(
    name = name, description = description, licence = licence,
    source = source, type = type
  )
  .pkgenv$resources <- rbind(.pkgenv$resources, resource)
}



#' Register or list indicators in mapme.biodiversity
#'
#' `register_indicator()` is used to register a new indicator function with base
#' information to the package's internal environment used to inform users about
#' available indicators. Note, registering a custom indicator will
#' only have effect for the current R session.
#' @param name A character vector indicating the name of the indicator.
#' @param description A character vector with a basic description
#' @param resources A character vector of the required resources
#'   that need to be available to calculate the indicator. The names must
#'   correspond with already registered resources.
#'
#' @return `register_indicator()` is called for the side-effect of registering
#'   an indicator
#' @name indicators
#' @export
#'
#' @examples
#' \dontrun{
#' register_indicator(
#'   name = "treecover_area",
#'   description = "Area of forest cover by year",
#'   resources = c(
#'     "gfw_treecover",
#'     "gfw_lossyear"
#'   )
#' )
#' }
register_indicator <- function(name = NULL, description = NULL, resources = NULL) {
  if (any(is.null(name), is.null(description), is.null(resources))) {
    stop("neither name, description nor resources can be NULL")
  }

  .check_char(name, "name")
  .check_char(description, "description")

  if (name %in% names(.pkgenv$indicators)) {
    warning(paste("indicator with name", name, "already registered"))
  }

  if (!inherits(resources, "character")) {
    stop("resources needs to be a charachter vector")
  }

  indicator <- tibble(name = name, description = description, resources = list(resources))
  .pkgenv$indicators <- rbind(.pkgenv$indicators, indicator)
}


#' Register or list resources in mapme.biodiversity
#'
#' `available_resources()` returns a tibble of registered resources with basic
#' information such as the source and the licence.
#'
#' @param resources If \code{NULL} returns a list of all resources (default).
#'   Otherwise only the ones specified.
#'
#' @return `available_resources()` returns a tibble listing available resources.
#' @name resources
#' @export
#' @include register.R
#' @examples
#' available_resources()
available_resources <- function(resources = NULL) {
  all_resources <- .pkgenv$resources

  if (is.null(resources)) {
    return(all_resources[order(all_resources[["name"]]), ])
  } else {
    if (any(!resources %in% all_resources[["name"]])) {
      not_avail <- which(!resources %in% names(all_resources))
      not_avail <- resources[not_avail]
      msg <- sprintf(
        "The following resources are not available:\n%s",
        paste(not_avail, collpase = " ")
      )
      stop(msg)
    }
    all_resources[all_resources[["name"]] %in% resources, ]
  }
}



#' Register or list indicators in mapme.biodiversity
#'
#' `available_indicators()` returns a tibble of registered indicators with basic
#' information such as the required resources.
#'
#' @param indicators If \code{NULL} returns a list of all registered indicators
#' (default). Otherwise only the ones specified.
#'
#' @return `available_resources()` returns a tibble listing available indicators.
#' @name indicators
#' @export
#' @include register.R
#' @examples
#' available_indicators()
available_indicators <- function(indicators = NULL) {
  all_indicators <- .pkgenv$indicators
  resources <- lapply(all_indicators[["resources"]], function(x) available_resources(x))
  all_indicators[["resources"]] <- resources

  if (is.null(indicators)) {
    return(all_indicators[order(all_indicators[["name"]]), ])
  } else {
    if (any(!indicators %in% all_indicators[["name"]])) {
      not_avail <- which(!indicators %in% names(all_indicators))
      not_avail <- indicators[not_avail]
      msg <- sprintf(
        "The following indicators are not available:\n%s",
        paste(not_avail, collpase = " ")
      )
      stop(msg)
    }
    all_indicators[all_indicators[["name"]] %in% indicators, ]
  }
}


.add_resource <- function(resource) {
  stopifnot(inherits(resource, "list"))
  stopifnot(!is.null(names(resource)))
  if (length(.pkgenv$avail_resources) == 0) {
    .pkgenv$avail_resources <- resource
  } else {
    name <- names(resource)
    .pkgenv$avail_resources[name] <- resource
  }
}

.avail_resources <- function(name) {
  return(.pkgenv$avail_resources)
}

.clear_resources <- function() {
  .pkgenv$avail_resources <- list()
}
