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
  urls <- unlist(sapply(target_years, function(year) .getPopCountURL(year)))

  # start download in a temporal directory within tmpdir
  if (verbose) pb <- progress_bar$new(total = length(urls))
  for (url in urls) {
    tryCatch(
      {
        if (verbose) pb$tick(0)
        download.file(url, file.path(rundir, basename(url)), quiet = TRUE)
        if (verbose) pb$tick()
      },
      error = function(e) {
        message("reading URLs!")
      }
    )
  }
  # return paths to the raster
  list.files(rundir, full.names = T)
}


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

