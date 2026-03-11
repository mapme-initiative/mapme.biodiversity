# Calculate precipitation sums based on CHIRPS

This functions allows to calculate precipitation sums based on the
CHIRPS rainfall estimates. Corresponding to the time-frame of the
analysis of the portfolio, monthly precipitation sums are calculated.

## Usage

``` r
calc_precipitation_chirps(years = 1981:2020, engine = "extract")
```

## Arguments

- years:

  A numeric vector indicating the years for which to calculate
  precipitation statistics.

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

## Value

A function that returns an indicator tibble with variable precipitation
and sum of precipitation (in mm) as value.

## Details

The required resources for this indicator are:

- [chirps](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/chirps.md)

## Examples

``` r
# \dontrun{
library(sf)
library(mapme.biodiversity)

outdir <- file.path(tempdir(), "mapme-data")
dir.create(outdir, showWarnings = FALSE)

mapme_options(
  outdir = outdir,
  verbose = FALSE
)

aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) %>%
  read_sf() %>%
  get_resources(get_chirps(years = 2010)) %>%
  calc_indicators(
    calc_precipitation_chirps(
      years = 2010,
      engine = "extract"
    )
  ) %>%
  portfolio_long()
#> Error in httr2::req_perform(httr2::request(chirps_url)) : 
#>   Failed to perform HTTP request.
#> Caused by error in `curl::curl_fetch_memory()`:
#> ! Timeout was reached [data.chc.ucsb.edu]:
#> Failed to connect to data.chc.ucsb.edu port 443 after 10001 ms: Timeout was reached
#> Error in .check_footprints(resource, resource_name): Download for resource chirps failed.
#> Returning unmodified portfolio.

aoi
#> Error: object 'aoi' not found
# }
```
