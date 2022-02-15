#' Backlog function for available resources
#'
#' This function returns a list of either all available ressources and some
#' additional metadata or for one or more requested resources. It can be used
#' by users of the package to inform themselves about the available data sets
#' and to learn about potentially additional arguments that should be specified
#' when requesting the resource.
#'
#' @param resources Defaults to NULL meaning that a list with all available
#'   resources will be returned. If a charachter vector is specified only the
#'   information about the requested resource will be returned.
#'
#' @return A list object.
#' @export
#'
available_resources <- function(resources = NULL){
  all_resources = list(
    "treecover" = list("type" = "raster",
                       "source" = "Global Forest Watch  (GFW)",
                       "downloader" = ".get_treecover",
                       "arguments" = list(
                         "vers" = "GFC-2018-v1.6")
    ),
    "lossyear" = list("type" = "raster",
                      "source" = "Global Forest Watch  (GFW)",
                      "downloader" = ".get_lossyear",
                      "arguments" = list(
                        "vers" = "GFC-2018-v1.6"
                      )
    )

  )

  # determine what to return
  if(is.null(resources)) {
    return(all_resources)
  } else {
    .check_requested_resources(resources)
    all_resources[resources]
  }

}
