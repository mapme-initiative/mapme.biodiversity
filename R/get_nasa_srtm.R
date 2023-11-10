#' NASADEM HGT v001
#'
#' This resource is processed by the Land Processes Distributed Active Archive
#' Center (LP DAAC) and made available at the Microsoft Planetery Computer.
#' NASADEM are distributed in 1 degree latitude by 1 degree longitude tiles and
#' consist of all land between 60° N and 56° S latitude. This accounts for about
#' 80% of Earth’s total landmass.
#'
#'
#' @name nasa_srtm
#' @docType data
#' @keywords resource
#' @format A global tiled raster resource available for most land areas.
#' @references NASA JPL (2020). NASADEM Merged DEM Global 1 arc second V001.
#'   NASA EOSDIS Land Processes DAAC. Accessed 2023-07-01 from
#'   https://doi.org/10.5067/MEaSUREs/NASADEM/NASADEM_HGT.001
#' @source https://planetarycomputer.microsoft.com/dataset/nasadem
NULL


#' Downloads SRTM 30m Digital Elevation Model (DEM) Layer
#'
#' @param x An sf object returned by init_portfolio
#' @param download_srtm Logical. Should SRTM tiles be downloaded or read
#'   from the server when computing. The latter might be more efficient for
#'   portfolios with a small spatial extent. Defaults to TRUE.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @keywords internal
#' @include register.R
#' @noRd
.get_nasa_srtm <- function(x,
                           rundir = tempdir(),
                           verbose = TRUE) {
  if (!requireNamespace("rstac", quietly = T)) {
    stop(paste0(
      "NASA SRTM resource requires rstac to be installed.\n",
      'Please run install.packages("rstac").'
    ))
  }

  urls <- try(rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
    rstac::stac_search(
      collection = "nasadem",
      bbox = as.numeric(st_bbox(x)),
      limit = NULL
    ) %>%
    rstac::post_request() %>%
    rstac::items_fetch() %>%
    rstac::assets_url(asset_names = "elevation"))

  if (inherits(urls, "try-error")) {
    stop("Download for NASA SRTM resource was unsuccesfull")
  }

  if (length(urls) == 0) {
    stop("The extent of the portfolio does not intersect with the SRTM grid.")
  }

  if (attr(x, "testing")) {
    return(basename(urls))
  }

  filenames <- file.path(rundir, basename(urls))
  # start download in a temporal directory within tmpdir
  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(
    urls = urls,
    filenames = filenames,
    verbose = verbose,
    aria_bin = aria_bin,
    check_existence = FALSE
  )
  filenames
}

register_resource(
  name = "nasa_srtm",
  type = "raster",
  source = "https://planetarycomputer.microsoft.com/dataset/nasadem",
  fun = .get_nasa_srtm,
  arguments <- list()
)
