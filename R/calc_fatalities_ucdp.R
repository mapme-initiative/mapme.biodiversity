#' Calculate number of fatalities of violent conflict from UCDP GED
#'
#' The indicator aggregated the number of fatalities within a given asset
#' on a monthly cadence stratified by the type of conflict. The different types
#' of conflicts encoded in the UCDP GED database are:
#' - state-based conflict
#' - non-state conflict
#' - one-sided violence
#'
#' The required resources for this indicator are:
#'  - [ucdp_ged]
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
#'   if two locations are mentioned a representative point in between is
#'   selected; if the location mentioned is an non-independent island; if
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
#' - value 2: if start and end dates for events are of unspecified character,
#'   spanning more than one calendar day though no longer than six days
#' - value 3: if when start and end dates for events are specified to a certain
#'   week, but specific dates are not provided
#' - value 4: if start and end dates for events are specified to a certain month
#' - value 5: if start and end dates for events are specified to a certain year,
#'   but specific dates are not provided
#'
#'
#' @name fatalities_ucpd
#' @param years A numeric vector indicating the years for which to summarize
#'    fatalities.
#' @param precision_location A numeric indicating precision value for the
#'   geolocation up to which events are included. Defaults to 1.
#' @param precision_time A numeric indicating the precision value of the
#'   temporal coding up to which events are included. Defaults to 1.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with the type of
#'   violence as variable and counts of civilian fatalities as value.
#' @references Sundberg, Ralph, and Erik Melander, 2013, “Introducing the UCDP
#'   Georeferenced Event Dataset”, Journal of Peace Research, vol.50, no.4, 523-532
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
#'   verbose = FALSE,
#'   chunk_size = 1e8
#' )
#'
#' aoi <- system.file("extdata", "burundi.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_ucdp_ged(version = "22.1")) %>%
#'   calc_indicators(
#'     calc_fatalities(
#'       years = 1991:1992,
#'       precision_location = 1,
#'       precision_time = 1
#'     )
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_fatalities_ucdp <- function(years = 1989:2023,
                                 precision_location = 1,
                                 precision_time = 1) {
  years <- check_available_years(years, c(1989:2023), "ucdp_ged")

  if (!precision_location %in% 1:7) {
    stop("Argument precision_location must be a single numeric between 1 and 7.")
  }

  if (!precision_time %in% 1:5) {
    stop("Argument precision_time must be a single numeric between 1 and 5.")
  }

  function(x,
           ucdp_ged = NULL,
           name = "fatalities_ucdp",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    date_prec <- where_prec <- date_start <- type_of_violence <- NULL
    year <- month <- deaths_a <- deaths_b <- event_count <- type_of_death <- NULL

    if (is.null(ucdp_ged)) {
      return(NULL)
    }
    ucdp_ged <- ucdp_ged[[1]]

    ucdp_ged <- ucdp_ged[unlist(st_contains(x, ucdp_ged)), ]
    ucdp_ged <- dplyr::select(
      st_drop_geometry(ucdp_ged),
      tidyr::starts_with("deaths_"),
      type_of_violence,
      date_prec,
      where_prec,
      date_start
    )

    ucdp_ged <- dplyr::filter(
      ucdp_ged,
      where_prec <= precision_location,
      date_prec <= precision_time
    )

    ucdp_ged <- dplyr::mutate(
      ucdp_ged,
      date_start = as.Date(date_start),
      year = format(date_start, "%Y")
    )
    ucdp_ged <- dplyr::filter(ucdp_ged, year %in% years)

    fatalities <- dplyr::summarise(
      ucdp_ged,
      dplyr::across(
        dplyr::starts_with("deaths_"),
        ~ sum(as.numeric(.x))
      ),
      event_count = dplyr::n(),
      .by = c(year, type_of_violence)
    )

    fatalities <- dplyr::mutate(
      fatalities,
      deaths_total = rowSums(dplyr::across(dplyr::starts_with("deaths_"))),
      year = as.Date(paste0(year, "-01-01"))
    ) %>%
      dplyr::select(-deaths_a, -deaths_b) %>%
      dplyr::relocate(event_count, .after = dplyr::last_col()) %>%
      dplyr::arrange(year, type_of_violence) %>%
      dplyr::mutate(type_of_violence = dplyr::case_when(
        type_of_violence == 1 ~ "state_based_conflict",
        type_of_violence == 2 ~ "non_state_conflict",
        type_of_violence == 3 ~ "one_sided_violence"
      )) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(cols = tidyr::starts_with("death"), names_to = "type_of_death") %>%
      dplyr::mutate(
        datetime = as.POSIXct(paste0(year, "T00:00:00Z")),
        variable = paste0("fatalities_", type_of_violence, "_", type_of_death),
        unit = "count",
        value = value
      ) %>%
      dplyr::select(datetime, variable, unit, value)

    fatalities
  }
}


register_indicator(
  name = "fatalities_ucdp",
  description = "Number of fatalities by group of conflict based on UCDP GED",
  resources = "ucdp_ged"
)
