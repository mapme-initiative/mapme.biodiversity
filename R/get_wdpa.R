#' World Database on Protected Areas
#'
#' This resource contains outlines of protected areas around the globe
#' which are part of the World Database on Protected Areas. This database
#' is compiled by UN Environment Programme World Conservation Monitoring Centre
#' (UNEP-WCMC), in collaboration with governments, non-governmental
#' organisations, academia and industry.
#'
#' The data is updated with a monthly cadence. Here, we only retain protected
#' areas with polygon information available, i.e. areas for which only
#' a point location is reported are omitted. Additionally, we apply
#' a function to repair geometries from GEOS wrap polygons that cross the
#' dateline.
#'
#' @name wdpa
#' @keywords resource
#' @param version A character vector indicating the version to be made available.
#'   Must contain the month as a three letter combination and the year, e.g. "Aug2024",
#'   or "latest" (the default) meaning the data from the previous month.
#' @returns A function that returns an `sf` footprints object.
#' @references UNEP-WCMC and IUCN (2024), Protected Planet: The World Database
#'   on Protected Areas (WDPA) 2024, Cambridge, UK: UNEP-WCMC and IUCN.
#'   Available at: www.protectedplanet.net.
#' @source \url{https://www.protectedplanet.net/}
#' @include register.R
#' @export
get_wdpa <- function(version = "latest") {
  if (version == "latest") {
    version <- .latest_wpda_version()
  }

  if (!grepl("^[A-Z]{1}[a-z]{2}[0-9]{4}$", version)) {
    stop("Version must consist of a three letter month abbreviation and the year, e.g. 'Aug2024'.")
  }

  function(
      x,
      name = "wdpa",
      type = "vector",
      outdir = mapme_options()[["outdir"]],
      verbose = mapme_options()[["verbose"]]) {
    bbox <- st_bbox(c(xmin = -180.0, xmax = 180.0, ymin = -80.0, ymax = 82.0), crs = "EPSG:4326")
    tile <- st_as_sf(st_as_sfc(st_bbox(bbox)))

    baseurl <- "https://pp-import-production.s3-eu-west-1.amazonaws.com/WDPA_WDOECM_%s_Public.zip"
    url <- file.path("/vsizip", "/vsicurl", sprintf(baseurl, version))
    filename <- sprintf("WDPA_WDOECM_%s_Public.gdb", version)
    src <- file.path(url, filename)

    layers <- sf::st_layers(src)
    layer <- grep("poly", layers$name, value = TRUE)

    dsn <- file.path(tempdir(), gsub("gdb", "gpkg", basename(src)))

    if (!is.null(outdir)) {
      if (spds_exists(file.path(outdir, basename(dsn)))) {
        return(make_footprints(file.path(outdir, basename(dsn)),
          what = "vector"
        ))
      }
    }

    sf::gdal_utils("vectortranslate",
      source = src,
      destination = dsn,
      options = c(
        layer,
        "-makevalid",
        "-wrapdateline"
      )
    )

    make_footprints(dsn, what = "vector")
  }
}

.latest_wpda_version <- function() {
  # version <- format(Sys.Date() - 30, "%b%Y")
  headers <- curlGetHeaders("http://wcmc.io/wdpa_current_release")
  url <- gsub("location: |\r\n", "", headers[grepl("^location", headers)])
  strsplit(url, "_")[[1]][3]
}

register_resource(
  name = "wdpa",
  description = "World Database on Protected Areas",
  licence = "https://www.protectedplanet.net/en/legal",
  source = "https://www.protectedplanet.net/",
  type = "vector"
)
