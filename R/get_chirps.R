#' Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)
#'
#' This resource is published by Funk et al. (2015) and represents a quasi-global
#' (50°S-50°S) rainfall estimation at a monthly resolution starting with the year
#' 1981 to the near-present. It has a spatial resolution of 0.05°. The data can
#' be used to retrieve information on the amount of rainfall. Due to the availability
#' of +30 years, anomaly detection and long-term average analysis is also possible.
#'
#' The following arguments should be set by the users:
#' \describe{
#'   \item{start_year}{A single integer value indicating the first year to be downloaded.}
#'   \item{end_year}{A single integer value indicating the last year to be downloaded.}
#' }
#'
#' @name chirps
#' @docType data
#' @keywords resource
#' @format Global raster layers available for years 1981 to near-present.
#' @source \url{https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs/}
#' @references Funk, C., Peterson, P., Landsfeld, M. et al. The climate hazards
#' infrared precipitation with stations—a new environmental record for
#' monitoring extremes. Sci Data 2, 150066 (2015).
#' \url{https://doi.org/10.1038/sdata.2015.66}
NULL



.get_chirps <- function(x,
                        start_year,
                        end_year,
                        rundir = tempdir(),
                        verbose = TRUE) {
  if (any(missing(start_year), missing(end_year))) {
    stop(paste("Please specify arguments 'start_year' and 'end_year' for ",
      "downloading resource 'chirps'",
      sep = ""
    ))
  }

  chirps_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs/%s"
  filename_stem <- "chirps-v2.0.%s.%s.cog"
  months <- 1:12
  months <- sapply(months, function(month) {
    ifelse(month < 10, paste0("0", month), paste(month))
  })
  years <- start_year:end_year
  filenames <- sprintf(filename_stem, rep(years, each = 12), rep(months, length(years)))
  urls <- sprintf(chirps_url, filenames)
  filenames <- file.path(rundir, filenames)

  .download_or_skip(urls, filenames, verbose = verbose, check_existence = TRUE)

  list.files(rundir, full.names = TRUE)
}
