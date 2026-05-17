# Calculate elevation statistics

This function allows to calculate elevation statistics for polygons. For
each polygon, the desired statistic(s) are returned.

## Usage

``` r
calc_elevation(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either one
  or multiple inputs as character "mean", "median" or "sd".

## Value

A function that returns an indicator tibble with specified elevation
statistics as variable and corresponding values (in meters) as value.

## Details

The required resources for this indicator are:

- [nasa_srtm](https://mapme-initiative.github.io/mapme.biodiversity/reference/nasa_srtm.md)

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
  get_resources(get_nasa_srtm()) %>%
  calc_indicators(
    calc_elevation(engine = "extract", stats = c("mean", "median", "sd", "var"))
  ) %>%
  portfolio_long()
#> Error : HTTP content type response 'text/plain' not defined for this operation.
#> Error in (function (x, name = "nasa_srtm", type = "raster", outdir = mapme_options()[["outdir"]],  : 
#>   Download for NASA SRTM resource was unsuccesfull
#> Error in .check_footprints(resource, resource_name): Download for resource nasa_srtm failed.
#> Returning unmodified portfolio.

aoi
#> Error: object 'aoi' not found
# }
```
