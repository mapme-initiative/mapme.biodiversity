.pkgenv <- new.env(parent = emptyenv())

.pkgenv$resources <- list()
.pkgenv$indicators <- list()


#' Register a new resource to the mapme.biodiversity
#'
#' This function can be used to register a custom resource function to be
#' available during the call of \code{get_resources}. Note, that registering
#' your own resource function will only have effect for the current R session.
#' If you return to your analysis in a new session, you will have to re-register
#' your custom resource.
#'
#' @param name A character vector indicating the name of the resource.
#' @param type A character vector indicating the type of the resource. Either
#'   'vector' or 'raster'.
#' @param source Optional, preferably a URL where the data is found.
#' @param fun The function you wish to register.
#' @param arguments A list with named entries indicating the default values
#'   for the arguments required by the function
#'
#' @return Nothing. Registers the function in the package environment.
#' @export
#'
#' @examples
#' \dontrun{
#' register_resource(
#'   name = "gfw_treecover",
#'   type = "raster",
#'   source = "https://data.globalforestwatch.org/documents/tree-cover-2000/explore",
#'   fun = .get_gfw_treecover,
#'   arguments = list(vers_treecover = "GFC-2021-v1.9")
#' )
#' }
register_resource <- function(name = NULL, type = NULL, source = NULL, fun = NULL, arguments = list()) {
  if (any(is.null(name), is.null(type), is.null(fun), is.null(arguments))) {
    stop("Neither name, type, fun or arguments can be NULL")
  }

  if (!inherits(name, "character") || length(name) > 1) {
    stop("name needs to be a length 1 character")
  }

  if (name %in% names(.pkgenv$resources)) {
    stop(paste("resource with name", name, "already registered"))
  }

  if (!type %in% c("vector", "raster")) {
    stop("type need to be one of 'vector' or 'raster'")
  }

  if ((!inherits(source, "character") || length(source) > 1) & !is.null(source)) {
    stop("source needs to be a length 1 character")
  }

  if (!inherits(fun, "function")) {
    stop("fun needs to be a valid function signature")
  }

  if (!inherits(arguments, "list")) {
    stop(paste(
      "arguments needs to be a list. Specify an empty list if your",
      "function does not have any arguments"
    ))
  }


  resource <- list(
    list(
      type = type,
      source = source,
      fun = match.fun(fun),
      arguments = arguments
    )
  )


  names(resource) <- name

  .pkgenv$resources <- append(.pkgenv$resources, resource)
}



#' Register a new indicator to mapme.biodiversity
#'
#' This function can be used to register a custom indicator function to be
#' available during the call of \code{calc_indicators}. Note, that registering
#' your own indicator function will only have effect for the current R session.
#' If you return to your analysis in a new session, you will have to re-register
#' your custom indicator.
#'
#' @param name A character vector indicating the name of the indicator.
#' @param resources A list with named objects indicating the resources
#'   that need to be available to calculate the indicator. The names correspond
#'   to registered resources and a single character value indicates the
#'   type of that resources
#' @param fun The function you wish to register.
#' @param arguments A list with named entries indicating the default values
#'   for the arguments required by the function
#' @param processing_mode A character vector indicating the preferred
#'   processing mode of the indicator. Either 'asset' or 'portfolio'.
#'
#' @return Nothing. Registers the function in the package environment.
#' @export
#'
#' @examples
#' \dontrun{
#' register_indicator(
#'   name = "treecover_arae",
#'   inputs = list(
#'     gfw_treecover = "raster",
#'     gfw_lossyear = "raster"
#'   ),
#'   fun = .calc_treecover_area,
#'   arguments = list(
#'     min_size = 10,
#'     min_cover = 30
#'   ),
#'   processing_mode = "asset"
#' )
#' }
register_indicator <- function(name = NULL, resources = NULL, fun = NULL,
                               arguments = NULL, processing_mode = NULL) {
  if (any(
    is.null(name), is.null(resources), is.null(fun), is.null(arguments),
    is.null(processing_mode)
  )) {
    stop("Neither name, resources, fun, arguments, or processing_mode can be NULL")
  }

  if (!inherits(name, "character") || length(name) > 1) {
    stop("name needs to be a length 1 character")
  }

  if (name %in% names(.pkgenv$indicators)) {
    stop(paste("indicator with name", name, "already registered"))
  }

  if (!inherits(fun, "function")) {
    stop("fun needs to be a valid function signature")
  }

  if (!inherits(arguments, "list")) {
    stop(paste(
      "arguments needs to be a list. Specify an empty list if your",
      "function does not have any arguments"
    ))
  }

  if (!processing_mode %in% c("asset", "portfolio")) {
    stop("processing_mode need to be one of 'asset' or 'portfolio'")
  }

  if (!inherits(resources, "list")) {
    stop(paste(
      "resources needs to be a list indicating the required resources",
      "and their types"
    ))
  }

  check_resources <- sapply(resources, function(x) x %in% c("vector", "raster"))
  if (any(!check_resources)) {
    wrong_types <- names(check_resources)[!check_resources]
    stop(paste(
      "The following resources have an unknown type specified: ",
      paste(wrong_types, sep = "", collapse = ", ")
    ))
  }

  indicator <- list(
    list(
      resources = resources,
      fun = match.fun(fun),
      arguments = arguments,
      processing_mode = processing_mode
    )
  )

  names(indicator) <- name

  .pkgenv$indicators <- append(.pkgenv$indicators, indicator)
}



#' Backlog function for available resources
#'
#' This function returns a list of either all available resources and some
#' additional metadata or for one or more requested resources. It can be used
#' by users of the package to inform themselves about the available data sets
#' and to learn about potentially additional arguments that should be specified
#' when requesting the resource.
#'
#' @param resources Defaults to NULL meaning that a list with all available
#'   resources will be returned. If a character vector is specified only the
#'   information about the requested resource will be returned.
#'
#' @return A list object.
#' @export
#' @keywords resource
#' @examples
#' names(available_resources())
available_resources <- function(resources = NULL) {
  all_resources <- .pkgenv$resources

  if (is.null(resources)) {
    return(all_resources[order(names(all_resources))])
  } else {
    .check_requested_resources(resources)
    all_resources[resources]
  }
}



#' Backlog function for available indicators
#'
#' This function returns a list of either all available indicators and some
#' additional metadata or for one or more requested indicators. It can be used
#' by users of the package to inform themselves about the available data sets
#' and to learn about potentially additional arguments that should be specified
#' when requesting the indicator.
#'
#' @param indicators Defaults to NULL meaning that a list with all available
#'   indicators will be returned. If a character vector is specified only the
#'   information about the requested indicator will be returned.
#'
#' @return A list object.
#' @export
#' @keywords indicator
#' @examples
#' names(available_indicators())
available_indicators <- function(indicators = NULL) {
  all_indicators <- .pkgenv$indicators

  if (is.null(indicators)) {
    return(all_indicators[order(names(all_indicators))])
  } else {
    .check_requested_indicator(indicators)
    all_indicators[indicators]
  }
}
