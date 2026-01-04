# Calculate slope statistics

This function allows to calculate slope statistics for polygons. For
each polygon, the desired statistic(s) are returned.

## Usage

``` r
calc_slope(engine = "exactextract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing function from either one of "zonal",
  "extract" or "exactextract" as a character string.

- stats:

  Function to be applied to compute statistics for polygons. Accepts
  either a single string or a vector of strings, such as "mean",
  "median", "sd", "min", "max", "sum", or "var".

## Value

A function that returns an indicator tibble with specified slope
statistics as variables and corresponding values (in degrees).

## Details

The required resource for this indicator is:

- [nasa_srtm](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/nasa_srtm.md)

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
    calc_slope(stats = c("mean", "median", "sd", "var"), engine = "extract")
  ) %>%
  portfolio_long()
#> Resource 'nasa_srtm' is already available.
#> Error : HTTP content type response 'text/html' not defined for this operation.
#> Error in (function (x, name = "nasa_srtm", type = "raster", outdir = mapme_options()[["outdir"]],  : 
#>   Download for NASA SRTM resource was unsuccesfull
#> Error in .check_footprints(resource, resource_name): Download for resource nasa_srtm failed.
#> Returning unmodified portfolio.

aoi
#> Error: object 'aoi' not found
# }
```
