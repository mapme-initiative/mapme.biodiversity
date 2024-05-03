#' Calculate active fire counts based on NASA FIRMS polygons
#'
#' This function allows to efficiently calculate the number of fire
#' events occurred in the region of interest from the NASA FIRMS active
#' fire polygon datasets. For each polygon, the fire event counts for
#' the desired year is returned.
#'
#' The required resources for this indicator are:
#'  - [nasa_firms]
#'
#' @name active_fire_counts
#' @keywords indicator
#' @returns A function that returns a tibble with a column for number of
#'   fire events per year and instrument.
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
#'   verbose = FALSE
#' )
#'
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_nasa_firms(years = 2021, instrument = "VIIRS")) %>%
#'   calc_indicators(calc_active_fire_counts()) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_active_fire_counts <- function() {
  function(x,
           nasa_firms = NULL,
           name = "active_fire_counts",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    acq_date <- NULL
    year <- NULL
    instrument <- NULL

    if (is.null(nasa_firms)) {
      return(NULL)
    }
    # select required columns and rbind objects
    nasa_firms <- lapply(nasa_firms, function(x) {
      dplyr::select(x, acq_date, instrument)
    }) %>%
      dplyr::bind_rows()

    intersected <- suppressWarnings(st_intersection(nasa_firms, st_geometry(x)))
    if (nrow(intersected) == 0) {
      return(NULL)
    }
    intersected %>%
      dplyr::select(-geom) %>%
      dplyr::as_tibble() %>%
      tidyr::separate(acq_date, c("year", "month", "day")) %>%
      dplyr::group_by(instrument, year) %>%
      dplyr::summarise(value = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        datetime = as.Date(paste0(year, "-01-01")),
        variable = paste0(tolower(instrument), "_fire_count"),
        unit = "count"
      ) %>%
      dplyr::select(datetime, variable, unit, value)
  }
}


register_indicator(
  name = "active_fire_counts",
  description = "Number of detected fires by NASA FIRMS",
  resources = "nasa_firms"
)
