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
#' - 18.1
#' - 19.1
#' - 20.1
#' - 21.1
#' - 22.1
#' - latest
#'
#' @name ucdp_ged
#' @param version A character vector specifying
#'   the version to download. Defaults to "latest".
#' @keywords resource
#' @returns A function that returns a character of file paths.
#' @references Davies, Shawn, Therese Pettersson & Magnus Öberg (2022).
#' Organized violence 1989-2021 and drone warfare.
#' Journal of Peace Research 59(4).
#' \doi{10.1177/00223433221108428}
#' @source \url{https://ucdp.uu.se/downloads/}
#' @importFrom utils unzip read.csv
#' @include register.R
#' @export
get_ucdp_ged <- function(version = "latest") {
  check_namespace("rvest")
  try(versions <- .ucdp_versions())
  if (inherits(versions, "try-error")) {
    stop("Available versions of UCDP GED could not be fetched")
  }
  if (version == "latest") version <- versions[length(versions)]
  if (!version %in% versions) {
    msg <- paste(versions, collapse = ", ")
    stop(paste0("Valid versions for UCDP GED: ", msg, "."))
  }

  function(x,
           name = "ucdp_ged",
           type = "vector",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    version_ged <- paste0("ged", gsub("\\.", "", version), "-csv.zip")

    base_url <- "/vsizip/vsicurl/https://ucdp.uu.se/downloads/ged/"
    url <- paste0(base_url, version_ged)
    if (version == "19.1") {
      url <- paste0(url, "/ged191.csv")
    } else if (version == "5.0") {
      url <- paste0(url, "/ged50.csv")
    }
    filename <- file.path(outdir, gsub("zip", "gpkg", version_ged))

    # return early if testing
    if (testing) {
      return(basename(filename))
    }

    if (file.exists(filename)) {
      return(filename)
    }

    gdal_utils(
      util = "vectortranslate",
      source = url,
      destination = filename,
      options = c(
        "-a_srs", "EPSG:4326",
        "-oo", "GEOM_POSSIBLE_NAMES=geom_wkt"
      )
    )
    filename
  }
}


.ucdp_versions <- function() {
  sections <- rvest::read_html("https://ucdp.uu.se/apidocs/") %>%
    rvest::html_node("body") %>%
    rvest::html_nodes("section")

  labels <- sections %>% rvest::html_attr("aria-label")
  target_p <- sections[which(labels == "Available datasets")]
  content <- target_p %>% rvest::html_nodes("p")
  versions <- rvest::html_text(content[2])
  versions <- strsplit(x = versions, "\\s+")[[1]]
  versions[versions != ""]
}


register_resource(
  name = "ucdp_ged",
  description = "UCDP Georeferenced Event Dataset (UCDP GED)",
  licence = "CC-BY 4.0",
  source = "https://ucdp.uu.se/downloads/",
  type = "vector"
)
