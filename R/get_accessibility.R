#' Downloads Accessibility to Cities layer
#'
#' Accessibility is the ease with which larger cities can be reached from a certain location.
#' This resource represents the travel time to major cities in the year 2015. Encoded as minutes,
#' representing the time needed to reach that particular cell from nearby city of population range
#' of interest.
#'
#' @param x An sf object returned by init_portfolio
#' @param range_accessibility The city within the defined range of population to download, defaults to \code{"20k_50k"}.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @name Travel_time_to_major_Cities
#' @keywords internal
#'

.get_accessibility <- function(x,
                               range_accessibility = "20k_50k",
                               rundir = tempdir(),
                               verbose = TRUE) {

  # get url for accessibility layer
  url <- .getaccessibilityURL(range_accessibility)
  filename = file.path(rundir, paste0("accessibility-", range_accessibility, ".tif"))
  # start download in a temporal directory within tmpdir
  .downloadOrSkip(url, filename, verbose)
  # return paths to the raster
  list.files(rundir, full.names = T)
}


#' Helper for accessibility urls generation
#'
#' @param range A valid range that is translated to an url
#'
#' @return A character string
#' @keywords internal
.getaccessibilityURL <- function(range) {
  df.index <- data.frame(
    range = c(
      "5k_10k", "10k_20k", "20k_50k", "50k_100k", "100k_200k", "200k_500k", "500k_1mio",
      "1mio_5mio", "50k_50mio", "5k_110mio", "20k_110mio"
    ),
    index = c(
      14189840, 14189837, 14189831, 14189825, 14189819, 14189816, 14189810,
      14189807, 14189849, 14189852, 14189843
    )
  )

  if(any(!range %in% df.index$range)){
    index = which(!range %in% df.index$range)
    basemsg = "The selected %s not available ranges for accessibility. Available ranges are %s."
    if(length(index) == 1){
      body = sprintf("range '%s' is", range[index])
    } else {
      body = sprintf("ranges '%s' are", paste(range[index], collapse = "', '"))
    }
    message(sprintf(basemsg, body, paste(df.index$range, collapse = ", ")))
    range = range[-index]
    if(length(range) == 0){
      stop("No supoorted ranges have been specified.")
    } else {
      basemsg = "Reduced to the available %s"
      if(length(range) == 1){
        body = sprintf("range of %s.", range)
      } else {
        body = sprintf("ranges of %s.", paste(range, collapse = ", "))
      }
      message(sprintf(basemsg, body))
    }
  }

  unlist(lapply(range, function(x){
    index = df.index$index[df.index$range == x]
    paste0("https://ndownloader.figshare.com/files/", index)
  }))
}

