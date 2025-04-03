#' Helper to create a grid of regular resolution and CRS
#'
#' Use this function to create a regular grid in a custom CRS. This is used
#' e.g. to create the tile grid for Global Forest Watch in order to retrieve
#' the intersecting tiles with a given portfolio.
#'
#' @param xmin minimum longitude value (E/W)
#' @param xmax maximum longitude value (E/W)
#' @param ymin minimum latitude value (S/N)
#' @param ymax maximum latitude value (E/W)
#' @param dx difference in longitude value per grid
#' @param dy difference in latitude value per grid
#' @param proj projection system
#' @returns An sf object with a defined grid.
#' @keywords utils
#' @export
make_global_grid <- function(xmin = -180, xmax = 170, dx = 10,
                             ymin = -50, ymax = 80, dy = 10,
                             proj = NULL) {
  if (is.null(proj)) proj <- st_crs(4326)
  ncells <- c(
    (xmax - xmin) / dx,
    (ymax - ymin) / dy
  )

  bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin))
  bbox <- st_as_sfc(bbox, crs = proj)
  st_as_sf(st_make_grid(bbox, n = ncells, crs = proj, what = "polygons"))
}

#' Helper to check yearly availability
#'
#' Use this function to check if a specified vector of years intersects
#' with the yearly availability of a resource.
#'
#' @param target_years Numeric indicating the target year.
#' @param available_years Numeric indicating the available years.
#' @param indicator A character vector with target resource/indicator name.
#' @keywords utils
#' @export
check_available_years <- function(target_years,
                                  available_years,
                                  indicator) {
  if (any(!target_years %in% available_years)) {
    target_years <- target_years[target_years %in% available_years]
    if (length(target_years) > 0) {
      message(sprintf("Some target years are not available for %s.", indicator))
    } else {
      stop(
        sprintf(
          "The target years do not intersect with the availability of %s.",
          indicator
        )
      )
    }
  }
  target_years
}

#' Checks if namespace is available
#'
#' Use this function if your resource/indicator function requires the
#' namespace of a certain package to be available. An informative error/warning
#' message is printed if that is not the case.
#'
#' @param pkg A character vector of length one indicating a package name
#'   for which the namespace is tested
#' @param error A logical indicating whether or not to promote missing namespace
#'   to error. If FALSE, a warning is emitted.
#'
#' @return TRUE, invisible, if the namespace is available. An error message
#'   if `error = TRUE`, FALSE and a warning otherwise.
#' @keywords utils
#' @export
check_namespace <- function(pkg, error = TRUE) {
  verb <- ifelse(error, "required", "recommended")
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste("R package '%s' %s.\n",
      "Please install via `install.packages('%s')`",
      sep = ""
    )
    msg <- sprintf(msg, pkg, verb, pkg)
    if (error) {
      stop(msg, .call = FALSE)
    } else {
      message(msg, .call = FALSE)
      return(invisible(FALSE))
    }
  }
  invisible(TRUE)
}

.has_internet <- function() {
  if (Sys.getenv("mapme_check_connection", unset = "TRUE") == "FALSE") {
    return(TRUE)
  }
  rsp <- httr2::req_perform(httr2::request("www.google.com"))
  has_internet <- !httr2::resp_is_error(rsp)
  if (!has_internet) {
    message("There seems to be no internet connection. Cannot download resources.")
  }
  has_internet
}

.geom_last <- function(data) {
  stopifnot(inherits(data, "sf"))
  sf_col <- attr(data, "sf_column")
  dplyr::relocate(data, !!sf_col,
    .after = dplyr::last_col()
  )
}
