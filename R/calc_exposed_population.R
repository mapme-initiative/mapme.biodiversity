#' Calculate population exposed to violent conflict from UCDP GED
#'
#' The indicator calculates the population exposed to conflict events within a
#' specified buffer distance around violent events in UCDP GED. Per default,
#' the first available WorldPop layer is used to estimate exposed populations
#' for years before the respective year, while the most recent layer is
#' used for years after.
#'
#' The indicator is inspired by the Conflict Exposure tool from ACLED (see
#' citation below), but differs in the regard that we simply flatten our
#' buffered event layer instead of applying voronoi tessellation.
#'
#' The required resources for this indicator are:
#'  - [ucdp_ged]
#'  - [worldpop]
#'
#' You may filter for certain types of violence. The coded types according
#' to the UCDP codebook are:
#' value 1: state-based conflict
#' value 2: non-state conflict
#' value 3: one-sided conflict
#'
#' You may apply quality filters based on the precision of the geolocation
#' of events and the temporal precision. By default, these are set to only
#' include events with the highest precision scores.
#'
#' For geo-precision there are levels 1 to 7 with decreasing accuracy:
#' - value 1: the location information corresponds exactly to the geographical
#'   coordinates available
#' - value 2: the location information refers to a limited area around a
#'   specified location
#' - value 3: the source refers to or can be specified to a larger location at
#'   the level of second order administrative divisions (ADM2), such as district
#'   or municipality, the GED uses centroid point coordinates for that ADM2.
#' - value 4: the location information refers to a first order administrative
#'   division, such as a province (ADM1), the GED uses the coordinates for the
#'   centroid point of ADM1
#' - value 5: is used in different cases if the source refers to parts of a
#'   country which are larger than ADM1, but smaller than the entire country;
#'   if two locations are mentioned a representiative point in between is
#'   selected; if the location mentioned is an non-independend island; if
#'   the location is not very specifically mentioned or in relation to another
#'   location
#' - value 6: the location mentioned refers to an entire country and its
#'   centroid is used
#' - value 7: If the event takes place over water or in international airspace,
#'   the geographical coordinates in the dataset either represent the centroid
#'   point of a certain water area or estimated coordinates
#'
#' For temporal precision there are levels 1 to 5 with decreasing precision:
#' - value 1: if the exact date of an event is known
#' - value 2: if start and enddates for events are of unspecified character,
#'   spanning more than one calendar day though no longer than six days
#' - value 3: if when start and end dates for events are specified to a certain
#'   week, but specific dates are not provided
#' - value 4: if start and end dates for events are specified to a certain month
#' - value 5: if start and enddates for events are specified to a certain year,
#'   but specific dates are not provided
#'
#'
#' @name exposed_population
#' @param distance A numeric of length 1 indicating the buffer size around
#'   included conflict events to calculate the exposed population.
#' @param violence_types A numeric vector indicating the types of violence
#'  to be included (see Details).
#' @param years A numeric vector indicating for which years to calculate
#'   the exposed population. Restricted to available years for UCDP GED.
#'   For years not intersecting with available WorldPop layers, the first layer
#'   is used for earlier years and the last layer to more recent years.
#' @param precision_location A numeric indicating precision value for the
#'   geolocation up to which events are included. Defaults to 1.
#' @param precision_time A numeric indicating the precision value of the
#'   temporal coding up to which events are included. Defaults to 1.
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with conflict exposure
#'   as variable and precentage of the population as its value.
#' @references Raleigh, C; C Dowd; A Tatem; A Linke; N Tejedor-Garavito; M
#'   Bondarenko and K Kishi. 2023. Assessing and Mapping Global and Local
#'   Conflict Exposure. Working Paper.
#' @include register.R
#' @export
#' @examples
#' \dontshow{
#' mapme.biodiversity:::.copy_resource_dir(file.path(tempdir(), "mapme-data"))
#' }
#' \dontrun{
#' if (FALSE) {
#'   library(sf)
#'   library(mapme.biodiversity)
#'
#'   outdir <- file.path(tempdir(), "mapme-data")
#'   dir.create(outdir, showWarnings = FALSE)
#'
#'   mapme_options(
#'     outdir = outdir,
#'     verbose = FALSE,
#'     chunk_size = 1e8
#'   )
#'
#'   aoi <- system.file("extdata", "burundi.gpkg",
#'     package = "mapme.biodiversity"
#'   ) %>%
#'     read_sf() %>%
#'     get_resources(
#'       get_ucdp_ged(version = "22.1"),
#'       get_worldpop(years = 2000)
#'     ) %>%
#'     calc_indicators(
#'       conflict_exposure(
#'         distance = 5000,
#'         violence_types = 1:3,
#'         years = 2000,
#'         precision_location = 1,
#'         precision_time = 1
#'       )
#'     ) %>%
#'     portfolio_long()
#'
#'   aoi
#' }
#' }
calc_exposed_population <- function(distance = 5000,
                                    violence_types = 1:3,
                                    years = c(1989:2023),
                                    precision_location = 1,
                                    precision_time = 1,
                                    engine = "extract") {
  stopifnot(length(distance) == 1 && distance > 0)
  if (!all(violence_types %in% 1:3)) {
    stop("Argument violence_types must be an numeric vector with values between 1 and 3.")
  }
  if (!precision_location %in% 1:7) {
    stop("Argument precision_location must be a single numeric between 1 and 7.")
  }
  if (!precision_time %in% 1:5) {
    stop("Argument precision_time must be a single numeric between 1 and 5.")
  }
  years <- check_available_years(years, c(1989:2023), "exposed_population")
  engine <- check_engine(engine)


  function(x,
           ucdp_ged = NULL,
           worldpop = NULL,
           name = "exposed_population",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    date_prec <- where_prec <- date_start <- year <- type_of_violence <- NULL

    if (is.null(ucdp_ged) | is.null(worldpop)) {
      return(NULL)
    }

    # filter ucdp
    ucdp_ged <- ucdp_ged[[1]]
    ucdp_ged <- ucdp_ged[unlist(st_contains(x, ucdp_ged)), ]
    if (nrow(ucdp_ged) == 0) {
      return(NULL)
    }

    ucdp_yearly <- ucdp_ged %>%
      dplyr::select(
        date_prec,
        where_prec,
        date_start,
        type_of_violence
      ) %>%
      dplyr::filter(
        where_prec <= precision_location,
        date_prec <= precision_time,
        type_of_violence %in% violence_types
      ) %>%
      dplyr::select(date_start) %>%
      dplyr::mutate(year = format(as.Date(date_start), "%Y")) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(geom = st_intersection(st_union(st_buffer(geom, dist = distance)), x))

    wpop_years <- as.numeric(substr(names(worldpop), 5, 8))

    exposures <- purrr::map(years, function(y) {
      ucdp <- dplyr::filter(ucdp_yearly, year == y)
      if (nrow(ucdp) == 0) {
        return(NULL)
      }
      if (y %in% wpop_years) {
        index <- which(wpop_years == y)
      } else {
        index <- ifelse(y < wpop_years[1], 1, length(wpop_years))
      }

      pop <- worldpop[[index]]
      exposed_pop <- select_engine(ucdp, pop, stats = "sum", engine = engine)[["sum"]]
      tibble::tibble(
        datetime = as.POSIXct(as.Date(paste0(y, "-01-01"), "%Y-%m-%d")),
        variable = "exposed_population",
        unit = "count",
        value = exposed_pop
      )
    })
    if (all(sapply(exposures, is.null))) {
      return(NULL)
    }
    purrr::list_rbind(exposures)
  }
}


register_indicator(
  name = "exposed_population",
  description = "Number of people exposed to conflicts based on UCDP GED",
  resources = c("ucdp_ged", "worldpop")
)
