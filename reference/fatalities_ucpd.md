# Calculate number of fatalities of violent conflict from UCDP GED

The indicator aggregated the number of fatalities within a given asset
on a monthly cadence stratified by the type of conflict. The different
types of conflicts encoded in the UCDP GED database are:

- state-based conflict

- non-state conflict

- one-sided violence

## Usage

``` r
calc_fatalities_ucdp(
  years = 1989:2023,
  precision_location = 1,
  precision_time = 1
)
```

## Arguments

- years:

  A numeric vector indicating the years for which to summarize
  fatalities.

- precision_location:

  A numeric indicating precision value for the geolocation up to which
  events are included. Defaults to 1.

- precision_time:

  A numeric indicating the precision value of the temporal coding up to
  which events are included. Defaults to 1.

## Value

A function that returns an indicator tibble with the type of violence as
variable and counts of civilian fatalities as value.

## Details

The required resources for this indicator are:

- [ucdp_ged](https://mapme-initiative.github.io/mapme.biodiversity/reference/ucdp_ged.md)

You may apply quality filters based on the precision of the geolocation
of events and the temporal precision. By default, these are set to only
include events with the highest precision scores.

For geo-precision there are levels 1 to 7 with decreasing accuracy:

- value 1: the location information corresponds exactly to the
  geographical coordinates available

- value 2: the location information refers to a limited area around a
  specified location

- value 3: the source refers to or can be specified to a larger location
  at the level of second order administrative divisions (ADM2), such as
  district or municipality, the GED uses centroid point coordinates for
  that ADM2.

- value 4: the location information refers to a first order
  administrative division, such as a province (ADM1), the GED uses the
  coordinates for the centroid point of ADM1

- value 5: is used in different cases if the source refers to parts of a
  country which are larger than ADM1, but smaller than the entire
  country; if two locations are mentioned a representative point in
  between is selected; if the location mentioned is an non-independent
  island; if the location is not very specifically mentioned or in
  relation to another location

- value 6: the location mentioned refers to an entire country and its
  centroid is used

- value 7: If the event takes place over water or in international
  airspace, the geographical coordinates in the dataset either represent
  the centroid point of a certain water area or estimated coordinates

For temporal precision there are levels 1 to 5 with decreasing
precision:

- value 1: if the exact date of an event is known

- value 2: if start and end dates for events are of unspecified
  character, spanning more than one calendar day though no longer than
  six days

- value 3: if when start and end dates for events are specified to a
  certain week, but specific dates are not provided

- value 4: if start and end dates for events are specified to a certain
  month

- value 5: if start and end dates for events are specified to a certain
  year, but specific dates are not provided

## References

Sundberg, Ralph, and Erik Melander, 2013, “Introducing the UCDP
Georeferenced Event Dataset”, Journal of Peace Research, vol.50, no.4,
523-532

## Examples

``` r
# \dontrun{
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
  get_resources(get_ucdp_ged(version = "22.1")) %>%
  calc_indicators(
    calc_fatalities(
      years = 1991:1992,
      precision_location = 1,
      precision_time = 1
    )
  ) %>%
  portfolio_long()
#> Error in calc_fatalities(years = 1991:1992, precision_location = 1, precision_time = 1): could not find function "calc_fatalities"

aoi
#> Error: object 'aoi' not found
# }
```
