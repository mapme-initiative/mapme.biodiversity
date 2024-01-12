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


#' @keywords internal
#' @include register.R
#' @noRd
.get_nelson_et_al <- function(x, range_traveltime, rundir = tempdir(), verbose = TRUE) {
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

  if (any(!range_traveltime %in% df_index$range)) {
    index <- which(!range_traveltime %in% df_index$range)
    basemsg <- paste("The selected %s not available ranges ",
      "for traveltime Available ranges are %s.",
      sep = ""
    )
    if (length(index) == 1) {
      body <- sprintf("range '%s' is", range_traveltime[index])
    } else {
      body <- sprintf("ranges '%s' are", paste(range_traveltime[index], collapse = "', '"))
    }
    if (verbose) message(sprintf(basemsg, body, paste(df_index$range, collapse = ", ")))
    range_traveltime <- range_traveltime[-index]
    if (length(range_traveltime) == 0) {
      stop("No supoorted ranges have been specified.")
    } else {
      basemsg <- "Reduced to the available %s"
      if (length(range_traveltime) == 1) {
        body <- sprintf("range of %s.", range_traveltime)
      } else {
        body <- sprintf("ranges of %s.", paste(range_traveltime, collapse = ", "))
      }
      if (verbose) message(sprintf(basemsg, body))
    }
  }

  index <- df_index$index[which(df_index$range %in% range_traveltime)]
  urls <- data$url[index]
  filenames <- file.path(rundir, paste0("travel_time_to_cities_", df_index$range[index], ".tif"))
  filenames <- download_or_skip(urls, filenames, verbose = verbose, check_existence = FALSE)
  make_footprints(filenames, what = "raster")
}


register_resource(
  name = "nelson_et_al",
  type = "raster",
  source = "https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3",
  fun = .get_nelson_et_al,
  arguments <- list(range_traveltime = "20k_50k")
)
