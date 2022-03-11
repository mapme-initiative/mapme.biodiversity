#' Downloads Population Count layer
#'
#' This resource represents the population count, layers available to download from the
#' year 2000 to 2020. The dataset is called as WorldPop Unconstrained Global Mosaics.
#' The encoded cell value represents the total number of people in that particular cell.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @name PopulationCount_Global_Mosaics
#' @keywords internal
#'

.get_popCount <- function(x,
                          rundir = tempdir(),
                          verbose = TRUE) {
  target_years <- attributes(x)$years
  available_years = 2000:2020
  target_years = .check_available_years(target_years, available_years, "popcount")
  urls <- unlist(sapply(target_years, function(year) .getPopCountURL(year)))
  filenames =  file.path(rundir, basename(urls))
  if(any(file.exists(filenames))) message("Skipping existing files in output directory.")
  # start download in a temporal directory within tmpdir
  .downloadOrSkip(urls, filenames, verbose)
  # return paths to the raster
  filenames
}


#' Helper function to concstruch population layer urls
#'
#' @param target_year A numeric indicating the target year
#'
#' @return A charchter vector.
#' @keywords internal
.getPopCountURL <- function(target_year) {
  available_years <- c(2000:2020)
  if (target_year %in% available_years) {
    url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/", target_year, "/0_Mosaicked/ppp_", target_year, "_1km_Aggregated.tif")
    url
  } else {
    warning(sprintf("Population count not available for target year %s", target_year))
    NULL
  }
}

