#' Calculate Proximity to Key Biodiversity Areas
#'
#' This function calculates the distance to the closest KBA within an area
#' of influence around an assets location.
#'
#' The required resources for this indicator are:
#'  - [key_biodiversity_areas_resource]
#'
#' Note, that for this indicator to function properly, two distinct geometry
#' columns in the portfolio are required. The active geometry column must
#' represent the area of influence, as the KBA resource will be clipped to
#' its extent during read time. A second inactive geometry columns must
#' be specified per the 'asset_column' parameter. It must not be equal
#' to the active geometry column.
#'
#' See the example code on how to calculate and activating a second geometry
#' column.
#'
#' @name proximity_kba
#' @docType data
#' @keywords indicator
#' @format A function returning an indicator tibble with `proximity_kba`
#'   as variable and the minimal distance (in meters) as value.
#' @include register.R
#' @export
#' @examples
#' \dontshow{
#' mapme.biodiversity:::.copy_resource_dir(file.path(tempdir(), "mapme-data"))
#' }
#' \dontrun{
#' library(sf)
#' library(mapme.biodiversity)
#'
#' outdir <- file.path(tempdir(), "mapme-data")
#' dir.create(outdir, showWarnings = FALSE)
#'
#' mapme_options(
#'   outdir = outdir,
#'   chunk_size = 1e8,
#'   verbose = FALSE
#' )
#'
#' aoi <- read_sf(
#'   system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'     package = "mapme.biodiversity"
#'   )
#' )
#'
#' asset_column <- attr(aoi, "sf_column")
#'
#' aoi$buffer <- st_geometry(st_buffer(aoi, dist = 50000))
#' st_geometry(aoi) <- "buffer"
#'
#' kbas <- system.file("res", "key_biodiversity_areas", "kbas.gpkg",
#'   package = "mapme.biodiversity"
#' )
#'
#' aoi <- get_resources(aoi, get_key_biodiversity_areas(kbas))
#' aoi <- calc_indicators(aoi, calc_proximity_kba(asset_column))
#'
#' aoi$buffer <- NULL
#' st_geometry(aoi) <- asset_column
#'
#' aoi <- portfolio_long(aoi)
#'
#' aoi
#' }
calc_proximity_kba <- function(asset_column = NULL) {
  if (is.null(asset_column)) {
    msg <- "'asset_column' must be set to a geometry column."
    stop(msg)
  }

  function(x = NULL,
           key_biodiversity_areas,
           name = "proximity_kba",
           mode = "asset",
           aggregation = "min",
           verbose = mapme_options()[["verbose"]]) {
    if (any(is.null(x), is.null(key_biodiversity_areas))) {
      return(NULL)
    }

    if (!inherits(x[[asset_column]], "sfc")) {
      stop("'asset_column' is expected to be of type 'sfc'.")
    }

    if (attr(x, "sf_column") == asset_column) {
      stop("The active geometry column cannot be the same as 'asset_column'.")
    }

    asset <- x[[asset_column]]
    buffer <- st_geometry(x)
    kbas <- key_biodiversity_areas[[1]]
    distance <- .calc_proximity(asset, kbas, buffer)

    if (is.null(distance)) {
      return(NULL)
    }

    tibble::tibble(
      datetime = as.POSIXct("2024-01-01T00:00:00Z"),
      variable = "proximity_kba",
      unit = "meters",
      value = distance
    )
  }
}

.calc_proximity <- function(x, y, b) {
  if (!length(y)) {
    return(NULL)
  }
  y <- suppressWarnings(st_intersection(y, b))
  if (nrow(y) == 0) return(NULL)
  y <- st_make_valid(y)
  dists <- as.numeric(unlist(st_distance(x, y)))
  min(dists)
}

register_indicator(
  name = "proximity_kba",
  description = "Distance (in meters) to the closest KBA within an area of influence.",
  resources = "key_biodiversity_areas"
)
