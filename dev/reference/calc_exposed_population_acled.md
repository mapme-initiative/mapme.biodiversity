# Calculate population exposed to violent conflict from ACLED

The indicator calculates the population exposed to conflict events
within a specified buffer distance around events in ACLED. Per default,
the first available WorldPop layer is used to estimate exposed
populations for years before the respective year, while the most recent
layer is used for years after.

## Usage

``` r
calc_exposed_population_acled(
  distance = 5000,
  filter_category = c("event_type", "sub_event_type", "disorder_type"),
  filter_types = NULL,
  years = c(1997:2024),
  precision_location = 1,
  precision_time = 1
)
```

## Arguments

- distance:

  A numeric vector indicating the buffer radius in meters. If length is
  1, the same buffer size around included conflict events is drawn.
  Otherwise, it must be equal to the length of included categories
  selected with `filter_types`.

- filter_category:

  A character indicating the categories to be used to calculate the
  exposed population by. Defaults to `event_type` meaning one estimation
  per event type will be returned.

- filter_types:

  A character vector of event types of the respective category specified
  in `filter_category` to retain. Defaults to NULL, meaning that no
  filter is applied and all types are retained.

- years:

  A numeric vector indicating for which years to calculate the exposed
  population. Restricted to available years for ACLED. For years not
  intersecting with available WorldPop layers, the first layer is used
  for earlier years and the last layer to more recent years.

- precision_location:

  A numeric indicating precision value for the geolocation up to which
  events are included. Defaults to 1.

- precision_time:

  A numeric indicating the precision value of the temporal coding up to
  which events are included. Defaults to 1.

## Value

A function that returns an indicator tibble with conflict exposure as
variable and percentage of the population as its value.

## Details

The indicator is inspired by the Conflict Exposure tool from ACLED (see
citation below), but differs in the regard that we simply flatten our
buffered event layer instead of applying voronoi tessellation.

The required resources for this indicator are:

- [acled](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/acled.md)

- [worldpop](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/worldpop.md)

Events in ACLED are classified according to the schema described
extensively in their codebook. You may filter for certain types of
events. The categories for which a filter can be applied are either
"event_type", "event_sub_type", or "disorder_type". These are translated
into the following categories:

- event_type:

  - battles

  - protests

  - riots

  - explosions/remote_violence

  - violence_against_civilians

  - strategic_developments

- event_sub_type:

  - government_regains_territory

  - non-state_actor_overtakes_territory

  - armed_clash

  - excessive_force_against_protesters

  - protest_with_intervention

  - peaceful_protest

  - violent_demonstration

  - mob_violence

  - chemical_weapon

  - air/drone_strike

  - suicide_bomb

  - shelling/artillery/missile_attack

  - remote_explosive/landmine/ied

  - grenade

  - sexual_violence

  - attack

  - abduction/forced_disappearance

  - agreement

  - arrests

  - change_to_group/activity

  - disrupted_weapons_use

  - headquarters_or_base_established

  - looting/property_destruction

  - non-violent_transfer_of_territory

  - other

- disorder_type:

  - political_violence

  - political_violence;\_demonstrations

  - demonstrations

  - political_violence

  - strategic_developments

You may supply buffer distances for each of the event categories. Custom
buffers will then be drawn per category. Supply a single value if you do
not wish do differentiate between categories. Otherwise, supply a vector
of distances equal to the length of included categories.

You may apply quality filters based on the precision of the geolocation
of events and the temporal precision. By default, these are set to only
include events with the highest precision scores.

For geo-precision there are levels 1 to 3 with decreasing accuracy:

- value 1: the source reporting indicates a particular town, and
  coordinates are available for that town

- value 2: the source material indicates that activity took place in a
  small part of a region, and mentions a general area or if an activity
  occurs near a town or a city, the event is coded to a town with
  geo-referenced coordinates to represent that area

- value 3: a larger region is mentioned, the closest natural location
  noted in reporting (like “border area,” “forest,” or “sea,” among
  others) – or a provincial capital is used if no other information at
  all is available

For temporal precision there are levels 1 to 3 with decreasing
precision:

- value 1: the source material includes an actual date of an event

- value 2: the source material indicates that an event happened sometime
  during the week or within a similar period of time

- value 3: the source material only indicates that an event took place
  sometime during a month (i.e. in the past two or three weeks, or in
  January), without reference to the particular date, the month
  mid-point is chosen

## References

Raleigh, C; C Dowd; A Tatem; A Linke; N Tejedor-Garavito; M Bondarenko
and K Kishi. 2023. Assessing and Mapping Global and Local Conflict
Exposure. Working Paper.

## Examples

``` r
# \dontrun{
if (FALSE) {
  library(sf)
  library(mapme.biodiversity)

  outdir <- file.path(tempdir(), "mapme-data")
  dir.create(outdir, showWarnings = FALSE)

  mapme_options(
    outdir = outdir,
    verbose = FALSE,
    chunk_size = 1e8
  )

  aoi <- system.file("extdata", "burundi.gpkg",
    package = "mapme.biodiversity"
  ) %>%
    read_sf() %>%
    get_resources(
      get_acled(year = 2000),
      get_worldpop(years = 2000)
    ) %>%
    calc_indicators(
      calc_exposed_population_acled(
        distance = 5000,
        years = 2000,
        precision_location = 1,
        precision_time = 1
      )
    ) %>%
    portfolio_long()

  aoi
}
# }
```
