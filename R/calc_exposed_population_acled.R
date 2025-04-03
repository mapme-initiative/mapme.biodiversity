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
#' in their codebook.
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
#' You may supply buffer distances for each of the event categories. Custom
#' buffers will then be drawn per category. Supply a single value if you
#' do not wish do differentiate between categories. Otherwise, supply a
#' vector of distances equal to the length of included categories.
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
#' @param distance A numeric vector indicating the buffer radius in meters.
#'   If length is 1, the same buffer size around included conflict events is
#'   drawn. Otherwise, it must be equal to the length of included categories
#'   selected with \code{filter_types}.
#' @param filter_category A character indicating the categories to be used
#'   to calculate the exposed population by. Defaults to `event_type` meaning
#'   one estimation per event type will be returned.
#' @param filter_types A character vector of event types of the respective
#'   category specified in `filter_category` to retain. Defaults to NULL,
#'   meaning that no filter is applied and all types are retained.
#' @param years A numeric vector indicating for which years to calculate
#'   the exposed population. Restricted to available years for ACLED.
#'   For years not intersecting with available WorldPop layers, the first layer
#'   is used for earlier years and the last layer to more recent years.
#' @param precision_location A numeric indicating precision value for the
#'   geolocation up to which events are included. Defaults to 1.
#' @param precision_time A numeric indicating the precision value of the
#'   temporal coding up to which events are included. Defaults to 1.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with conflict exposure
#'   as variable and percentage of the population as its value.
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
    filter_category = c("event_type", "sub_event_type", "disorder_type"),
    filter_types = NULL,
    years = c(1997:2024),
    precision_location = 1,
    precision_time = 1) {
  check_namespace("exactextractr")
  filter_category <- match.arg(filter_category)
  if (filter_category == "event_type") {
    types <- .acled_event_types
  } else if (filter_category == "sub_event_type") {
    types <- .acled_sub_event_types
  } else {
    types <- .acled_disorder_types
  }
  if (!is.null(filter_types)) {
    stopifnot(all(filter_types %in% types))
  }

  if (!precision_location %in% 1:3) {
    stop("Argument precision_location must be a single numeric between 1 and 3.")
  }
  if (!precision_time %in% 1:3) {
    stop("Argument precision_time must be a single numeric between 1 and 3.")
  }

  if (length(distance) > 1) {
    if (is.null(filter_category)) {
      stop("filter_category cannot be NULL if per-class distances are specified.")
    }
    if (is.null(filter_types)) {
      if (length(types) != length(distance)) {
        msg <- c(
          "Wrong number of distances specified for filter_category `%s`!\n",
          "Total number of categories is %s."
        )
        stop(sprintf(msg, c(filter_category, length(types))))
      }
    } else {
      if (length(filter_types) != length(distance)) {
        msg <- c(
          "Wrong number of distances specified for filter_types!\n",
          "Total number of selected categories is %d."
        )
        stop(sprintf(msg, length(filter_types)))
      }
    }
  }

  years <- check_available_years(years, c(1997:2024), "exposed_population_acled")

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
    acled <- dplyr::filter(
      acled,
      year %in% years,
      geo_precision <= precision_location,
      time_precision <= precision_time
    )

    if (nrow(acled) == 0) {
      return(NULL)
    }

    acled <- dplyr::select(
      acled,
      year,
      stratum = !!filter_category
    )

    acled <- dplyr::mutate(
      acled,
      stratum = gsub(" ", "_", tolower(stratum))
    )

    if (is.null(filter_types)) {
      acled$distance <- distance
    } else {
      acled <- dplyr::filter(acled, stratum %in% filter_types)
      distance_df <- data.frame(
        stratum = filter_types,
        distance = distance
      )
      acled <- dplyr::left_join(acled, distance_df, by = "stratum")
    }

    if (nrow(acled) == 0) {
      return(NULL)
    }

    acled <- st_buffer(acled, acled$distance)

    # one multi-polygon per year and stratum
    acled_yearly <- dplyr::select(acled, year, stratum) %>%
      dplyr::group_by(year, stratum) %>%
      dplyr::summarise(geom = st_intersection(st_make_valid(st_union(geom)), x))

    # one multi-polygon per year, irrespective of stratum (total exposed pop)
    acled_yearly_total <- dplyr::summarise(acled_yearly, geom = st_intersection(st_make_valid(st_union(geom)), x))
    acled_yearly_total$stratum <- "total"
    acled_yearly <- rbind(acled_yearly, acled_yearly_total)

    .calc_exp_pop(
      acled_yearly,
      worldpop,
      years
    )
  }
}

.calc_exp_pop <- function(polys, popr, years) {
  year <- NULL
  stopifnot("year" %in% names(polys))
  wpop_years <- as.numeric(substr(names(popr), 5, 8))

  exposures <- purrr::map(years, function(y) {
    polys_year <- dplyr::filter(polys, year == y)
    if (nrow(polys_year) == 0) {
      return(NULL)
    }
    if (y %in% wpop_years) {
      index <- which(wpop_years == y)
    } else {
      index <- ifelse(y < wpop_years[1], 1, length(wpop_years))
    }

    pop <- popr[[index]]
    exposed_pop <- exactextractr::exact_extract(
      pop, polys_year,
      fun = "sum", progress = FALSE
    )

    tibble::tibble(
      datetime = as.POSIXct(as.Date(paste0(y, "-01-01"), "%Y-%m-%d")),
      variable = paste0("exposed_population_", polys_year$stratum),
      unit = "count",
      value = exposed_pop
    )
  })

  if (all(sapply(exposures, is.null))) {
    return(NULL)
  }
  purrr::list_rbind(exposures)
}

.acled_categories <- c(
  "event_type",
  "sub_event_type",
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
