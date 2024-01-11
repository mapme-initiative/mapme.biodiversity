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
#' The following arguments can be set:
#' \describe{
#'   \item{precision_location}{A numeric indicating precision value for the
#'   geolocation up to which events are included. Defaults to 1.}
#'   \item{precision_time}{A numeric indicating the precision value of the
#'   temporal coding up to which events are included. Defaults to 1.}
#' }
#'
#' @name fatalities
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for the date (year and month), the type
#'  of violence an counts of civilian fatalities, unknown fatalities and the
#'  total sum of fatalities.
#' @references Sundberg, Ralph, and Erik Melander, 2013, “Introducing the UCDP
#'   Georeferenced Event Dataset”, Journal of Peace Research, vol.50, no.4, 523-532
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
#' aoi <- system.file("extdata", "burundi.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 1991:1992,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("ucdp_ged", version_ged = "22.1") %>%
#'   calc_indicators("fatalities",
#'     precision_location = 1, precision_time = 1
#'   ) %>%
#'   tidyr::unnest(fatalities)
#'
#' aoi
#' }
NULL

#' Calculate fatalities from UCDP GED
#'
#' Filters the UCDP GED database based on the location and temporal
#' precison code as well as the temporal window under investigation.
#' Then fatalities are aggregated on a monthly time scale. Missing months
#' are filled with zeros.
#'
#' @param x A single polygon for which to calculate the tri statistic
#' @param ucdp_ged An sf object of the intersecting part of the UCDP database.
#' @param precision_location A numeric indicating precision value for the
#'   geolocation up to which events are included. Defaults to 1.
#' @param precision_time A numeric indicating the precision value of the
#'   temporal coding up to which events are included. Defaults to 1.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_fatalities <- function(x,
                             ucdp_ged,
                             precision_location = 1,
                             precision_time = 1,
                             verbose = TRUE,
                             ...) {
  date_prec <- where_prec <- date_start <- type_of_violence <- NULL
  year <- month <- deaths_a <- deaths_b <- event_count <- NULL

  ucdp_ged <- ucdp_ged[[1]]
  if (length(ucdp_ged) == 0) {
    return(NA)
  }

  target_years <- attributes(x)$years
  available_years <- c(1989:2023)
  target_years <- .check_available_years(
    target_years, available_years, "ucdp_ged"
  )

  if (!precision_location %in% 1:7) {
    stop("Argument precision_location must be a single numeric between 1 and 7.")
  }

  if (!precision_time %in% 1:5) {
    stop("Argument precision_time must be a single numeric between 1 and 5.")
  }

  months_tibble <- tidyr::expand_grid(
    year = as.character(target_years),
    month = sprintf("%02d", 1:12),
    type_of_violence = as.character(1:3)
  )

  ucdp_ged %>%
    st_drop_geometry() %>%
    dplyr::select(
      tidyr::starts_with("deaths_"),
      type_of_violence,
      date_prec,
      where_prec,
      date_start
    ) %>%
    dplyr::filter(
      where_prec <= precision_location,
      date_prec <= precision_time
    ) %>%
    dplyr::select(-date_prec, -where_prec) %>%
    dplyr::mutate(
      date_start = as.Date(date_start),
      year = format(date_start, "%Y"),
      month = format(date_start, "%m")
    ) %>%
    dplyr::filter(year %in% target_years) %>%
    dplyr::summarise(
      dplyr::across(
        tidyselect::starts_with("deaths_"),
        ~ sum(as.numeric(.x))
      ),
      event_count = dplyr::n(),
      .by = c(year, month, type_of_violence)
    ) %>%
    dplyr::right_join(months_tibble, by = c("year", "month", "type_of_violence")) %>%
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with(c("deaths_", "event_")),
      ~ tidyr::replace_na(.x, 0)
    )) %>%
    dplyr::mutate(
      deaths_total = rowSums(dplyr::across(tidyselect::starts_with("deaths_")))
    ) %>%
    dplyr::mutate(month = as.Date(paste0(year, "-", month, "-01"))) %>%
    dplyr::select(-year, -deaths_a, -deaths_b) %>%
    dplyr::relocate(event_count, .after = tidyselect::last_col()) %>%
    dplyr::arrange(month, type_of_violence) %>%
    dplyr::mutate(type_of_violence = dplyr::case_when(
      type_of_violence == 1 ~ "state-based conflict",
      type_of_violence == 2 ~ "non-state conflict",
      type_of_violence == 3 ~ "one-sided violence"
    )) %>%
    tibble::as_tibble()
}


register_indicator(
  name = "fatalities",
  resources = list(ucdp_ged = "vector"),
  fun = .calc_fatalities,
  arguments = list(
    precision_location = 1,
    precision_time = 1
  ),
  processing_mode = "asset"
)
