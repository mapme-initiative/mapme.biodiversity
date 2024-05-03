#' mapme.biodiversity
#'
#' @name mapme.biodiversity - package
#' @docType package
#' @title mapme.biodiversity - Global biodiversity portfolio analysis
#' @import sf terra
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr "%>%"
#' @importFrom utils download.file
#' @noRd
#' @keywords internal
globalVariables(c(":=", "!!", ".id", ".", "datetime", "variable", "unit", "value"))
NULL


.copy_resource_dir <- function(target) {
  if (!dir.exists(target)) {
    dir.create(target, showWarnings = FALSE)
    resource_dir <- system.file("res", package = "mapme.biodiversity")
    for (dir in list.dirs(resource_dir)) {
      file.copy(dir, target, recursive = TRUE)
    }
  }
}
