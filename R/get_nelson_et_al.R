#' Accessibility to Cities layer
#'
#' This resource is published by Weiss et al. (2018) "A global map of travel
#' time to cities to assess inequalities in accessibility in 2015" on journal
#' nature. Accessibility is the ease with which larger cities can be reached
#' from a certain location. This resource represents the travel time to major
#' cities in the year 2015. Encoded as minutes, representing the time needed
#' to reach that particular cell from nearby city of target population range.
#' The following ranges to nearby cities are available:
#' - "5k_10k"
#' - "10k_20k"
#' - "20k_50k"
#' - "50k_100k"
#' - "100k_200k"
#' - "200k_500k"
#' - "500k_1mio"
#' - "1mio_5mio"
#' - "50k_50mio"
#' - "5k_110mio"
#' - "20k_110mio"
#' - "5mio_50mio"
#'
#' The following argument should be specified by users:
#'
#' \describe{
#'   \item{range_traveltime}{A character vector indicating one or more ranges
#'   to download.}
#'   }
#'
#' @name nelson_et_al
#' @docType data
#' @keywords resource
#' @format Global raster layer available for year 2015.
#' @references Weiss, D. J., Nelson, A., Gibson, H. S., Temperley, W., Peedell,
#' S., Lieber, A., … & Gething, P. W. (2018). A global map of travel time to cities
#' to assess inequalities in accessibility in 2015. Nature, 553(7688), 333-336.
#' @source \url{https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3}
NULL


#' Downloads Accessibility to Cities layer
#'
#' @param x An sf object returned by init_portfolio
#' @param range_traveltime The city within the defined range of population
#'   to download, defaults to \code{"20k_50k"}.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @keywords internal
#' @include register.R
#' @noRd
.get_nelson_et_al <- function(x,
                              range_traveltime = "20k_50k",
                              rundir = tempdir(),
                              verbose = TRUE) {
  filenames <- file.path(
    rundir,
    paste0("traveltime-", range_traveltime, ".tif")
  )
  # get url for accessibility layer
  check <- .get_traveltime_url(range_traveltime, filenames, verbose = verbose)
  urls <- check$urls
  filenames <- check$filenames
  if (attr(x, "testing")) {
    return(basename(filenames))
  }
  # start download in a temporal directory within tmpdir
  aria_bin <- attributes(x)$aria_bin
  if (!is.null(attr(x, "testing"))) .download_or_skip(urls, filenames, verbose, check_existence = FALSE, aria_bin = aria_bin)
  # return paths to the raster
  filenames
}


#' Helper for traveltime urls generation
#'
#' @param range A valid range that is translated to an url
#'
#' @return A character string
#' @keywords internal
#' @noRd
.get_traveltime_url <- function(range, filenames, verbose = TRUE) {
  df_index <- data.frame(
    range = c(
      "5mio_50mio", "1mio_5mio", "500k_1mio", "200k_500k", "100k_200k",
      "50k_100k", "20k_50k", "10k_20k", "5k_10k", "20k_110mio", "50k_50mio",
      "5k_110mio"),
    index = 1:12
  )

  baseurl <- "https://api.figshare.com/v2/articles/7638134/files"
  cnt <- httr::GET(baseurl, httr::content_type_json()) |> httr::content()
  data <- lapply(cnt, function(x) data.frame(filename = x[["name"]], url = x[["download_url"]]))
  data <- do.call(rbind, data)
  data <- data[grep("to_cities", data[["filename"]]), ]
  data <- data[grep("tif", data[["filename"]]), ]

  if (any(!range %in% df_index$range)) {
    index <- which(!range %in% df_index$range)
    basemsg <- paste("The selected %s not available ranges ",
      "for traveltime Available ranges are %s.",
      sep = ""
    )
    if (length(index) == 1) {
      body <- sprintf("range '%s' is", range[index])
    } else {
      body <- sprintf("ranges '%s' are", paste(range[index], collapse = "', '"))
    }
    if (verbose) message(sprintf(basemsg, body, paste(df_index$range, collapse = ", ")))
    range <- range[-index]
    filenames <- filenames[-index]
    if (length(range) == 0) {
      stop("No supoorted ranges have been specified.")
    } else {
      basemsg <- "Reduced to the available %s"
      if (length(range) == 1) {
        body <- sprintf("range of %s.", range)
      } else {
        body <- sprintf("ranges of %s.", paste(range, collapse = ", "))
      }
      if (verbose) message(sprintf(basemsg, body))
    }
  }

  index <- df_index$index[which(df_index$range %in% range)]
  urls <- data$url[index]
  filenames <- file.path(rundir, paste0("travel_time_to_cities_", df_index$range[index], ".tif"))
  filenames <- download_or_skip(urls, filenames, verbose = verbose, check_existence = FALSE)
  fps <- make_footprints(filenames, what = "raster")
  fps[["filename"]] <- basename(filenames)
  fps
}



register_resource(
  name = "nelson_et_al",
  type = "raster",
  source = "https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3",
  fun = .get_nelson_et_al,
  arguments <- list(range_traveltime = "20k_50k")
)
