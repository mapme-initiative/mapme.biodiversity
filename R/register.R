#' Backlog function for available resources
#'
#' Returns a list of resource names and parametrization options for the
#' specified resources. If no resource names are provided, it lists all
#' available resources, including custom registered ones.
#' Use it to learn about possible additional arguments that can be specified
#' when requesting a resource.
#'
#' @param resources If \code{NULL} returns a list of all resources (default).
#'   Otherwise only the ones specified.
#'
#' @return A list object.
#' @export
#' @keywords resource
#' @include register.R
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


register_resource <- function(...) NULL
register_indicator <- function(...) NULL
