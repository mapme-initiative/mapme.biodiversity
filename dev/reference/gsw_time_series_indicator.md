# Calculate Global Surface Water Time Series

This function calculates the total area of the global surface water time
series data, separated by the following classes:

## Usage

``` r
calc_gsw_time_series()
```

## Format

A function returning a tibble with time series of global surface water
data classes.

## Details

- No Observation: It was not possible to determine whether a pixel was
  water (this may be the case for frozen areas or during the polar night
  in extreme latitudes).

- Permanent Water: Water was detected in twelve months per year or in a
  combination of permanent and no observation.

- Seasonal Water: Water and no water was detected.

- No Water: No Water was detected.

The required resources for this indicator are:

- [gsw_time_series_resource](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gsw_time_series_resource.md)

## Examples

``` r
# \dontrun{
library(mapme.biodiversity)
library(sf)

outdir <- file.path(tempdir(), "mapme-data")
dir.create(outdir, showWarnings = FALSE)

mapme_options(
  outdir = outdir,
  verbose = FALSE
)

aoi <- read_sf(
  system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
              package = "mapme.biodiversity"
))
aoi <- get_resources(aoi, get_gsw_time_series (years = 2000:2001))
aoi <- calc_indicators(aoi, calc_gsw_time_series())
aoi <- portfolio_long(aoi)

aoi
#> Simple feature collection with 8 features and 10 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -59.84866 ymin: 8.307999 xmax: -59.71 ymax: 8.364002
#> Geodetic CRS:  WGS 84
#> # A tibble: 8 × 11
#>   WDPAID NAME     DESIG_ENG ISO3  assetid indicator datetime            variable
#>    <dbl> <chr>    <chr>     <chr>   <int> <chr>     <dttm>              <chr>   
#> 1  41057 Shell B… Managed … GUY         1 gsw_time… 2000-01-01 00:00:00 no_obse…
#> 2  41057 Shell B… Managed … GUY         1 gsw_time… 2001-01-01 00:00:00 no_obse…
#> 3  41057 Shell B… Managed … GUY         1 gsw_time… 2000-01-01 00:00:00 not_wat…
#> 4  41057 Shell B… Managed … GUY         1 gsw_time… 2001-01-01 00:00:00 not_wat…
#> 5  41057 Shell B… Managed … GUY         1 gsw_time… 2000-01-01 00:00:00 seasona…
#> 6  41057 Shell B… Managed … GUY         1 gsw_time… 2001-01-01 00:00:00 seasona…
#> 7  41057 Shell B… Managed … GUY         1 gsw_time… 2000-01-01 00:00:00 permane…
#> 8  41057 Shell B… Managed … GUY         1 gsw_time… 2001-01-01 00:00:00 permane…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
