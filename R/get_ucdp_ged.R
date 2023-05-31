#' UCDP Georeferenced Event Dataset (UCDP GED)
#'
#' This resource distributed by the Uppsala Conflict Data Program (UCDP) constitutes
#' its most diaggregated dataset on individual events of organized violence.
#' It encodes the different actors involved, is spatially disaggregated down
#' to village levels anc currently covers the time period of 1989 to 2021.
#' Older versions of the data set can be downloaded, but users are recommended
#' to download the latest data set.
#'
#' The following versions are available
#' - 5.0
#' - 17.1
#' - 17.2
#' - 18.1"
#' - 19.1
#' - 20.1
#' - 21.1
#' - 22.1 or latest
#'
#' The following argument should be specified by users:
#'
#' \describe{
#'   \item{version_ged}{A character vector specifying
#'   the version to download. Defaults to "latest".}
#'   }
#'
#' @name ucdp_ged
#' @docType data
#' @keywords resource
#' @format A global event dataset (GED) encoding envents of organized
#'   violence a point geometries
#' @references Davies, Shawn, Therese Pettersson & Magnus Öberg (2022).
#' Organized violence 1989-2021 and drone warfare.
#' Journal of Peace Research 59(4).
#' \doi{10.1177/00223433221108428}
#' @source \url{https://ucdp.uu.se/downloads/}
NULL


#' Downloads UCDP GED dataset and transforms to sf
#'
#' @param x An sf object returned by init_portfolio
#' @param version_ged A character vector specifying the version of GED to
#'   download. Defaults to "latest".
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip read.csv
#' @keywords internal
#' @noRd
.get_ucdp_ged <- function(x,
                          version_ged = "latest",
                          rundir = tempdir(),
                          verbose = TRUE) {
  versions <- .ucdp_versions()

  if (version_ged == "latest") version_ged <- versions[length(versions)]

  if (!version_ged %in% versions) {
    msg <- paste(versions, collapse = ", ")
    stop(paste0("Valid versions for UCDP GED: ", msg, "."))
  }

  version <- paste0("ged", stringr::str_remove_all(version_ged, "\\."), "-csv.zip")

  base_url <- "https://ucdp.uu.se/downloads/ged/"
  download_url <- paste0(base_url, version)
  filename <- file.path(rundir, version)

  # return early if testing
  if (attr(x, "testing")) {
    return(basename(filename))
  }

  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(download_url,
    filename,
    verbose = verbose,
    check_existence = FALSE,
    aria_bin = aria_bin
  )

  # read data and transform to sf
  csv <- unzip(filename, list = TRUE)$Name
  data <- read.csv(unz(filename, csv))
  data <- as_tibble(data)
  data <- st_as_sf(data, wkt = "geom_wkt")
  st_crs(data) <- st_crs("EPSG:4326")

  gpkg <- file.path(rundir, stringr::str_replace(version, ".zip", ".gpkg"))
  write_sf(data, gpkg, delete_dsn = TRUE)
  gpkg
}


.ucdp_versions <- function() {
  sections <- rvest::read_html("https://ucdp.uu.se/apidocs/") %>%
    rvest::html_node("body") %>%
    rvest::html_nodes("section")

  labels <- sections %>% rvest::html_attr("aria-label")
  target_p <- sections[which(labels == "Available datasets")]
  content <- target_p %>% rvest::html_nodes("p")
  versions <- rvest::html_text(content[2])
  versions <- stringr::str_extract_all(versions, "\\d+(?:\\.\\d+)+")[[1]]
  versions
}
