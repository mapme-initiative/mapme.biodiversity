# Calculate accessibility statistics

Accessibility is the ease with which larger cities can be reached from a
certain location. This function allows to efficiently calculate
accessibility statistics (i.e. travel time to nearby major cities) for
polygons. For each polygon, the desired statistic/s (mean, median or sd)
is/are returned.

## Usage

``` r
calc_traveltime(engine = "extract", stats = "mean")
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

A function that returns an indicator tibble with city ranges and
statistics as variable and corresponding values (in minutes) as value.

## Details

The required resources for this indicator are:

- [nelson_et_al](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/nelson_et_al.md)

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
  get_resources(get_nelson_et_al(ranges = "100k_200k")) %>%
  calc_indicators(
    calc_traveltime(engine = "extract", stats = c("min", "max"))
  ) %>%
  portfolio_long()

aoi
#> Simple feature collection with 2 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 9
#>   WDPAID ISO3  assetid indicator  datetime            variable       unit  value
#>    <dbl> <chr>   <int> <chr>      <dttm>              <chr>          <chr> <dbl>
#> 1 478140 DOM         1 traveltime 2015-01-01 00:00:00 100k_200k_tra… minu…   162
#> 2 478140 DOM         1 traveltime 2015-01-01 00:00:00 100k_200k_tra… minu…   528
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
