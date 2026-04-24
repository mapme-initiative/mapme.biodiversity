# Calculate accessibility statistics for the year 2000

Accessibility refers to the ease with which cities can be reached from a
certain location. This function allows efficient calculation of
accessibility statistics (i.e., travel time to the nearest city) for
polygons. For each polygon, the desired statistic/s (mean, median or sd)
is/are returned.

## Usage

``` r
calc_traveltime_2000(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either
  single or multiple inputs as character. Supported statistics are:
  "mean", "median", "sd", "min", "max", "sum", "var".

## Value

A function that returns an indicator tibble with accessibility
statistics for the year 2000 as variables and corresponding values (in
minutes) as values.

## Details

The required resource for this indicator is:

- [accessibility_2000](https://mapme-initiative.github.io/mapme.biodiversity/reference/accessibility_2000.md)

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
  get_resources(get_accessibility_2000()) %>%
  calc_indicators(
    calc_traveltime_2000(stats = c("mean", "median", "sd"), engine = "extract")
  ) %>%
  portfolio_long()

aoi
#> Simple feature collection with 3 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 3 × 9
#>   WDPAID ISO3  assetid indicator       datetime            variable  unit  value
#>    <dbl> <chr>   <int> <chr>           <dttm>              <chr>     <chr> <dbl>
#> 1 478140 DOM         1 traveltime_2000 2000-01-01 00:00:00 travelti… minu…  387.
#> 2 478140 DOM         1 traveltime_2000 2000-01-01 00:00:00 travelti… minu…  420 
#> 3 478140 DOM         1 traveltime_2000 2000-01-01 00:00:00 travelti… minu…  204.
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
