#' Calculate number of fatalities of conflict events from ACLED
#'
#' The indicator aggregated the number of fatalities within a given asset
#' on a monthly cadence stratified either by event type, sub-event type or disorder
#' type. To learn about the different categorisation ACLED uses to encode events
#' please consult ACLED's codebook.
#'
#' The required resources for this indicator are:
#'  - [acled]
#'
#' You may apply quality filters based on the precision of the geolocation
#' of events and the temporal precision. By default, these are set to only
#' include events with the highest precision scores.
#'
#' For geo-precision there are levels 1 to 3 with decreasing accuracy:
#' - value 1: the source reporting indicates a particular town, and coordinates
#'   are available for that town
#' - value 2: the source material indicates that activity took place in a
#'   small part of a region, and mentions a general area or if an activity
#'   occurs near a town or a city, the event is coded to a town with
#'   geo-referenced coordinates to represent that area
#' - value 3: a larger region is mentioned, the closest natural location noted
#'   in reporting (like “border area,” “forest,” or “sea,” among others) – or a
#'   provincial capital is used if no other information at all is available
#'
#' For temporal precision there are levels 1 to 3 with decreasing precision:
#' - value 1: the source material includes an actual date of an event
#' - value 2: the source material indicates that an event happened sometime
#'   during the week or within a similar period of time
#' - value 3: the source material only indicates that an event took place
#'   sometime during a month (i.e. in the past two or three weeks, or in
#'   January), without reference to the particular date, the month mid-point
#'   is chosen
#'
#' @name fatalities_acled
#' @param years A numeric vector indicating the years for which to summarize
#'    fatalities.
#' @param stratum A character vector indicating the stratification to be applied.
#'   Should be one of "event_type", "sub_event_type", or "disorder_type".
#'   Defaults to "event_type".
#' @param precision_location A numeric indicating precision value for the
#'   geolocation up to which events are included. Defaults to 1.
#' @param precision_time A numeric indicating the precision value of the
#'   temporal coding up to which events are included. Defaults to 1.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with the type of
#'   violence as variable and counts of civilian fatalities as value.
#' @references Raleigh, C., Kishi, R. & Linke, A. Political instability patterns
#'   are obscured by conflict dataset scope conditions, sources, and coding
#'   choices. Humanit Soc Sci Commun 10, 74 (2023).
#'   \doi{https://doi.org/10.1057/s41599-023-01559-4}
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
#'   get_resources(get_acled(years = 2020)) %>%
#'   calc_indicators(
#'     calc_fatalities_acled(
#'       years = 2020,
#'       precision_location = 1,
#'       precision_time = 1
#'     )
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_fatalities_acled <- function(
    years = 2000,
    stratum = c("event_type", "sub_event_type", "disorder_type"),
    precision_location = 1,
    precision_time = 1) {
  years <- check_available_years(years, c(1997:2024), "acled")
  stratum <- match.arg(stratum)

  if (!precision_location %in% 1:3) {
    stop("Argument precision_location must be a single numeric between 1 and 3.")
  }

  if (!precision_time %in% 1:3) {
    stop("Argument precision_time must be a single numeric between 1 and 3.")
  }

  function(x,
           acled = NULL,
           name = "fatalities_acled",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    time_precision <- geo_precision <- event_date <- year <- month <- NULL

    if (is.null(acled)) {
      return(NULL)
    }

    acled <- st_sf(purrr::list_rbind(acled))
    acled <- acled[unlist(st_contains(x, acled)), ]
    acled <- dplyr::filter(acled, year %in% years)

    if (nrow(acled) == 0) {
      return(NULL)
    }

    acled <- dplyr::select(
      st_drop_geometry(acled),
      event_date,
      time_precision,
      geo_precision,
      fatalities,
      stratum = !!stratum
    )

    acled <- dplyr::filter(
      acled,
      time_precision <= precision_location,
      geo_precision <= precision_time
    )

    acled <- dplyr::mutate(
      acled,
      event_date = as.Date(event_date),
      year = format(event_date, "%Y")
    )

    fatalities <- dplyr::ungroup(
      dplyr::summarise(
        dplyr::group_by(
          acled,
          year, stratum,
        ),
        fatalities = sum(as.numeric(fatalities), na.rm = TRUE)
      )
    )

    fatalities_total <- fatalities %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(fatalities = sum(as.numeric(fatalities), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(stratum = "total")

    fatalities <- rbind(fatalities, fatalities_total)

    fatalities <- dplyr::mutate(
      fatalities,
      datetime = as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"),
      variable = paste0("fatalities_", gsub(" ", "_", tolower(stratum))),
      unit = "count",
      value = fatalities
    )

    dplyr::select(fatalities, datetime, variable, unit, value)
  }
}


register_indicator(
  name = "fatalities_acled",
  description = "Number of fatalities by event type based on ACLED.",
  resources = "acled"
)
