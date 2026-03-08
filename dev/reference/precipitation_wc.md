# Calculate precipitation statistics

This function allows to efficiently calculate precipitation statistics
from Worldclim for polygons. For each polygon, the desired statistic/s
(min, max, sum, mean, median, sd or var) is/are returned.

## Usage

``` r
calc_precipitation_wc(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either
  single or multiple inputs as character. Supported statistics are:
  "mean", "median", "sd", "min", "max", "sum" "var".

## Value

A function that returns an indicator tibble with precipitation
statistics as variable and corresponding values as value.

## Details

The required resources for this indicator are:

- precipitation layer from
  [worldclim_precipitation](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/worldclim_precipitation.md)

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
  get_resources(get_worldclim_precipitation(years = 2018)) %>%
  calc_indicators(
    calc_precipitation_wc(
      engine = "extract",
      stats = c("mean", "median")
    )
  ) %>%
  portfolio_long()
#> Warning: GDAL Message 1: HTTP response code on https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.09//wc2.1_cruts4.09_2.5m_prec_2010-2019.zip: 0
#> ***** WARNING *****
#> 12 out of 12 resources are not available for 'worldclim_precipitation'
#> Error in (function (.x, .f, ..., .progress = FALSE) {    map_("list", .x, .f, ..., .progress = .progress)})(.x = 1L, .f = function (...) {    {        ...furrr_chunk_seeds_i <- ...furrr_chunk_seeds_env[["i"]]        ...furrr_chunk_seeds_env[["i"]] <- ...furrr_chunk_seeds_i +             1L        assign(x = ".Random.seed", value = ...furrr_chunk_seeds[[...furrr_chunk_seeds_i]],             envir = globalenv(), inherits = FALSE)    }    NULL    ...furrr_out <- ...furrr_fn(...)    ...furrr_out}): ℹ In index: 1.
#> Caused by error in `purrr::map()`:
#> ℹ In index: 1.
#> Caused by error:
#> ! Resource worldclim_precipitation is empty

aoi
#> Error: object 'aoi' not found
# }
```
