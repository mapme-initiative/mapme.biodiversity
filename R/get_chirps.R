#' Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)
#'
#' This resource is published by Funk et al. (2015) and represents a quasi-global
#' (50°S-50°S) rainfall estimation at a monthly resolution starting with the year
#' 1981 to the near-present. It has a spatial resolution of 0.05°. The data can
#' be used to retrieve information on the amount of rainfall. Due to the availability
#' of +30 years, anomaly detection and long-term average analysis is also possible.
#' The routine will download the complete archive in order to support long-term
#' average and anomaly calculations with respect to the 1981 - 2010 climate normal
#' period. Thus no additional arguments need to be specified.
#'
#'
#' @name chirps
#' @keywords resource
#' @returns A function that returns a character of file paths.
#' @source \url{https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs/}
#' @references Funk, C., Peterson, P., Landsfeld, M. et al. The climate hazards
#' infrared precipitation with stations—a new environmental record for
#' monitoring extremes. Sci Data 2, 150066 (2015).
#' \doi{10.1038/sdata.2015.66}
#' @include register.R
#' @export
get_chirps <- function() {
  function(x,
           name = "chirps",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    chirps_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/"

    try(chirps_list <- rvest::read_html(chirps_url) %>%
      rvest::html_elements("a") %>%
      rvest::html_text2())

    if (inherits(chirps_list, "try-error")) {
      stop("Download for CHIRPS resource was unsuccesfull")
    }

    chirps_list <- grep("*.tif.gz$", chirps_list, value = TRUE)
    urls <- paste(chirps_url, chirps_list, sep = "")
    filenames <- file.path(outdir, basename(urls))

    if (testing) {
      return(basename(filenames))
    }

    filenames <- download_or_skip(urls, filenames, check_existence = FALSE)
    filenames <- purrr::walk(filenames, unzip_and_remove,
      dir = outdir, remove = FALSE
    )
    gsub(".gz", "", filenames)
  }
}

register_resource(
  name = "chirps",
  description = "Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)",
  licence = "CC - unknown",
  source = "https://www.chc.ucsb.edu/data/chirps",
  type = "raster"
)
