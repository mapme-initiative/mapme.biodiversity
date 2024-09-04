#' Calculate population exposed to violent conflict from ACLED
#'
#' The indicator calculates the population exposed to conflict events within a
#' specified buffer distance around events in ACLED. Per default,
#' the first available WorldPop layer is used to estimate exposed populations
#' for years before the respective year, while the most recent layer is
#' used for years after.
#'
#' The indicator is inspired by the Conflict Exposure tool from ACLED (see
#' citation below), but differs in the regard that we simply flatten our
#' buffered event layer instead of applying voronoi tessellation.
#'
#' The required resources for this indicator are:
#'  - [acled]
#'  - [worldpop]
#'
#' Events in ACLED are classified according to the schema described extensively
#' in their codebook (\url{https://acleddata.com/knowledge-base/codebook/}).
#' You may filter for certain types of events. The categories for which a
#' filter can be applied are either "event_type", "event_sub_type", or
#' "disorder_type". These are translated into the following categories:
#'
#' - event_type:
#'   - battles
#'   - protests
#'   - riots
#'   - explosions/remote_violence
#'   - violence_against_civilians
#'   - strategic_developments
#'
#' - event_sub_type:
#'   - government_regains_territory
#'   - non-state_actor_overtakes_territory
#'   - armed_clash
#'   - excessive_force_against_protesters
#'   - protest_with_intervention
#'   - peaceful_protest
#'   - violent_demonstration
#'   - mob_violence
#'   - chemical_weapon
#'   - air/drone_strike
#'   - suicide_bomb
#'   - shelling/artillery/missile_attack
#'   - remote_explosive/landmine/ied
#'   - grenade
#'   - sexual_violence
#'   - attack
#'   - abduction/forced_disappearance
#'   - agreement
#'   - arrests
#'   - change_to_group/activity
#'   - disrupted_weapons_use
#'   - headquarters_or_base_established
#'   - looting/property_destruction
#'   - non-violent_transfer_of_territory
#'   - other
#'
#' - disorder_type:
#'   - political_violence
#'   - political_violence;_demonstrations
#'   - demonstrations
#'   - political_violence
#'   - strategic_developments
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
#' @name calc_exposed_population_acled
#' @param distance A numeric of length 1 indicating the buffer size around
#'   included conflict events to calculate the exposed population.
#' @param filter_category A character indicating the category of events
#'   to which apply a filter. Defaults to NULL, meaning that no filter
#'   is applied.
#' @param filter_types A character vector of event types of the respective
#'   category to retain. Defaults to NULL, meaning that no filter is applied.
#' @param years A numeric vector indicating for which years to calculate
#'   the exposed population. Restricted to available years for ACLED.
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
#'       get_acled(year = 2000),
#'       get_worldpop(years = 2000)
#'     ) %>%
#'     calc_indicators(
#'       conflict_exposure_acled(
#'         distance = 5000,
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
calc_exposed_population_acled <- function(
    distance = 5000,
    filter_category = NULL,
    filter_types = NULL,
    years = c(1997:2024),
    precision_location = 1,
    precision_time = 1,
    engine = "extract") {
  stopifnot(length(distance) == 1 && distance > 0)

  if (!is.null(filter_category)) {
    stopifnot(filter_category %in% .acled_categories)
    if (filter_category == "event_type") {
      types <- .acled_event_types
    } else if (filter_category == "event_sub_type") {
      types <- .acled_sub_event_types
    } else {
      types <- .acled_disorder_types
    }
    if (!is.null(filter_types)) {
      stopifnot(all(filter_types %in% types))
    }
  }

  if (!precision_location %in% 1:3) {
    stop("Argument precision_location must be a single numeric between 1 and 3.")
  }
  if (!precision_time %in% 1:3) {
    stop("Argument precision_time must be a single numeric between 1 and 3.")
  }
  years <- check_available_years(years, c(1997:2024), "exposed_population_acled")
  engine <- check_engine(engine)

  function(x,
           acled = NULL,
           worldpop = NULL,
           name = "exposed_population_acled",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    time_precision <- geo_precision <- event_date <- year <- stratum <- NULL

    if (is.null(acled) | is.null(worldpop)) {
      return(NULL)
    }

    acled <- st_sf(purrr::list_rbind(acled))
    acled <- acled[unlist(st_contains(x, acled)), ]
    acled <- dplyr::filter(acled, year %in% years)

    if (nrow(acled) == 0) {
      return(NULL)
    }

    acled <- dplyr::select(
      acled,
      time_precision,
      geo_precision,
      event_date,
      stratum = !!filter_category
    )

    acled <- dplyr::filter(
      acled,
      geo_precision <= precision_location,
      time_precision <= precision_time
    )

    if ("stratum" %in% names(acled)) {
      acled <- dplyr::mutate(
        acled,
        stratum = gsub(" ", "_", tolower(stratum))
      ) %>%
        dplyr::filter(stratum %in% filter_types)
    }

    if (nrow(acled) == 0) {
      return(NULL)
    }

    acled_yearly <- dplyr::select(acled, event_date) %>%
      dplyr::mutate(year = format(as.Date(event_date), "%Y")) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(geom = st_intersection(st_make_valid(st_union(st_buffer(geom, dist = distance))), x))

    wpop_years <- as.numeric(substr(names(worldpop), 5, 8))

    exposures <- purrr::map(years, function(y) {
      acled <- dplyr::filter(acled_yearly, year == y)
      if (nrow(acled) == 0) {
        return(NULL)
      }
      if (y %in% wpop_years) {
        index <- which(wpop_years == y)
      } else {
        index <- ifelse(y < wpop_years[1], 1, length(wpop_years))
      }

      pop <- worldpop[[index]]
      exposed_pop <- select_engine(acled, pop, stats = "sum", engine = engine)[["sum"]]
      tibble::tibble(
        datetime = as.POSIXct(as.Date(paste0(y, "-01-01"), "%Y-%m-%d")),
        variable = "exposed_population_acled",
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

.acled_categories <- c(
  "event_type",
  "event_sub_type",
  "disorder_type"
)

.acled_event_types <- c(
  "battles",
  "protests",
  "riots",
  "explosions/remote_violence",
  "violence_against_civilians",
  "strategic_developments"
)

.acled_disorder_types <- c(
  "political_violence",
  "political_violence;_demonstrations",
  "demonstrations",
  "political_violence",
  "strategic_developments"
)

.acled_sub_event_types <- c(
  "government_regains_territory",
  "non-state_actor_overtakes_territory",
  "armed_clash",
  "excessive_force_against_protesters",
  "protest_with_intervention",
  "peaceful_protest",
  "violent_demonstration",
  "mob_violence",
  "chemical_weapon",
  "air/drone_strike",
  "suicide_bomb",
  "shelling/artillery/missile_attack",
  "remote_explosive/landmine/ied",
  "grenade",
  "sexual_violence",
  "attack",
  "abduction/forced_disappearance",
  "agreement",
  "arrests",
  "change_to_group/activity",
  "disrupted_weapons_use",
  "headquarters_or_base_established",
  "looting/property_destruction",
  "non-violent_transfer_of_territory",
  "other"
)


register_indicator(
  name = "exposed_population_acled",
  description = "Number of people exposed to conflicts based on ACLED",
  resources = c("acled", "worldpop")
)
