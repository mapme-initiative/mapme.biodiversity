#' Support for group_by for mapme_portfolios
#' @name tidyverse
#' @param .data data object of class \link{mapme_portfolio}
#' @param ... other arguments
#' @export
group_by.mapme_portfolio <- function(.data, ...) {
  atts = attributes(.data)
  class(.data) <- setdiff(class(.data), "mapme_portfolio")
  as_portfolio(NextMethod(), atts)
}

#' Support for group_split for mapme_portfilios
#' @name tidyverse
#' @export
group_split.mapme_portfolio <- function(.data, ...) {
  atts = attributes(.data)
  class(.data) =  setdiff(class(.data), c("mapme_portfolio"))
  # lapply(dplyr::group_split(.tbl, ..., .keep = .keep), function(x) as_portfolio(x, atts))
  as_portfolio(NextMethod(), atts)
}

#' support for lists of mapme_portfolios e.g. output from group_split
#'
#' @param ls a list of mapme_portfolio objects
#' @keywords internal
as_portfolio.list <- function(ls, atts){
  lapply(ls, function(x) as_portfolio(x, atts))
}
