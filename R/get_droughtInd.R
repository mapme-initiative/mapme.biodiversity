#' Downloads NASA GRACE-based Drought Indicator Layer
#'
#' This resource indicates the drought, global raster layer available from year
#' 2003 to present with weekly updates. Encoded as percentile, representing current
#' wet or dry conditions showing the probability of occurrence - with lower values
#' meaning dryer than normal, and higher values meaning wetter than normal.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @name Drought_Indicator
#' @keywords internal
#'

.get_droughtInd <- function(x,
                            rundir = tempdir(),
                            verbose = TRUE) {
  target_years <- attributes(x)$years
  available_years = 2003:2022
  target_years = .check_available_years(target_years, available_years, "droughtindicators")

  urls <- unlist(sapply(target_years, function(year) .getDroughtIndURL(year)))
  filenames = file.path(rundir, basename(urls))
  if(any(file.exists(filenames))) message("Skipping existing files in output directory.")
  # start download in a temporal directory within rundir
  .downloadOrSkip(urls, filenames, verbose)
  list.files(rundir, full.names = T)
}


#' Helper function to construct GRACE urls
#'
#' @param target_year
#'
#' @return A character vector
#' @keywords internal
.getDroughtIndURL <- function(target_year) {
  available_years <- c(2003:2022)
  dates <- seq.Date(as.Date("2003/02/03"), as.Date("2022/02/28"), by = "week")
  available_dates <- as.integer(format(dates, "%Y%m%d"))
  if (target_year %in% available_years) {
    target_dates <- subset(available_dates, substr(available_dates, 1, 4) == target_year)
    url <- paste0("https://nasagrace.unl.edu/globaldata/", target_dates, "/gws_perc_025deg_GL_", target_dates, ".tif")
    url
  } else {
    warning(sprintf("Drought indicator not available for target year %s", target_year))
    NULL
  }
}
