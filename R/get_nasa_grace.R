#' NASA GRACE-based Drought Indicator layer
#'
#' The resource is published by NASA GRACE Tellus. This data set
#' reflects on potential drought conditions in the shallow groundwater section
#' relative to a reference period spanning from 1948 to 2012.
#' It is available as a global raster with a weekly temporal resolution starting
#' with the year 2003. The value indicates the wetness percentile of a given
#' pixel with regard to the reference period.
#'
#' @name nasa_grace
#' @docType data
#' @keywords resource
#' @format Global raster layers available for years 2003 to present.
#' @source \url{https://nasagrace.unl.edu/globaldata/}
NULL


#' Downloads NASA GRACE-based Drought Indicator Layer
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @keywords internal
#' @noRd

.get_nasa_grace <- function(x,
                            rundir = tempdir(),
                            verbose = TRUE) {
  target_years <- attributes(x)$years
  available_years <- 2003:2022
  target_years <- .check_available_years(
    target_years, available_years, "droughtindicators"
  )

  urls <- unlist(sapply(target_years, function(year) .get_nasagrace_url(year)))
  filenames <- file.path(rundir, basename(urls))
  if (attr(x, "testing")) {
    return(basename(filenames))
  }
  # start download in a temporal directory within rundir
  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(urls, filenames, verbose, aria_bin = aria_bin, check_existence = FALSE)
  filenames
}


#' Helper function to construct GRACE urls
#'
#' @param target_year
#'
#' @return A character vector
#' @keywords internal
.get_nasagrace_url <- function(target_year) {
  available_years <- c(2003:2022)
  dates <- seq.Date(as.Date("2003/02/03"), as.Date("2022/05/16"), by = "week")
  available_dates <- as.integer(format(dates, "%Y%m%d"))
  if (target_year %in% available_years) {
    target_dates <- subset(
      available_dates, substr(available_dates, 1, 4) == target_year
    )
    paste0(
      "https://nasagrace.unl.edu/globaldata/", target_dates,
      "/gws_perc_025deg_GL_", target_dates, ".tif"
    )
  } else {
    warning(sprintf(
      "Drought indicator not available for target year %s",
      target_year
    ))
    NULL
  }
}
