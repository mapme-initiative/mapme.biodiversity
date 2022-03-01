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

  # get url
  url <- .getaccessibilityURL(range_accessibility)
  # start download in a temporal directory within tmpdir
  if (verbose) pb <- progress_bar$new(total = length(url))
  if (verbose) pb$tick(0)
  download.file(url, file.path(rundir, basename(paste0("accessibility-", range_accessibility, ".tif"))), quiet = TRUE)
  if (verbose) pb$tick()

  # return paths to the raster
  list.files(rundir, full.names = T)
}


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

  if (range %in% df.index$range) {
    index <- df.index$index[df.index$range == range]
    url <- paste0("https://ndownloader.figshare.com/files/", index)
    url
  } else {
    warning(sprintf("Accessibility layer not available for range %s", range))
    NULL
  }
}

