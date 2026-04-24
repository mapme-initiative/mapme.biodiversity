# Calculate population count statistics

WorldPop, which was initiated in 2013, offers easy access to spatial
demographic datasets, claiming to use peer-reviewed and fully
transparent methods to create global mosaics for the years 2000 to 2020.
This function allows to efficiently calculate population count
statistics (e.g. total number of population) for polygons. For each
polygon, the desired statistic/s (min, max, sum, mean, median, sd or
var) is/are returned.

## Usage

``` r
calc_population_count(engine = "extract", stats = "sum")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either one
  or multiple inputs as character "min", "max", "sum", "mean", "median"
  "sd" or "var".

## Value

A function that returns an indicator tibble with the specified
populations statistics as variable and the corresponding values as
value.

## Details

The required resources for this indicator are:

- [worldpop](https://mapme-initiative.github.io/mapme.biodiversity/reference/worldpop.md)

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
  get_resources(get_worldpop(years = 2010:2020)) %>%
  calc_indicators(
    calc_population_count(engine = "extract", stats = c("sum", "median"))
  ) %>%
  portfolio_long()

aoi
#> Simple feature collection with 22 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 22 × 9
#>    WDPAID ISO3  assetid indicator      datetime            variable unit   value
#>     <dbl> <chr>   <int> <chr>          <dttm>              <chr>    <chr>  <dbl>
#>  1 478140 DOM         1 population_co… 2010-01-01 00:00:00 populat… count 4016. 
#>  2 478140 DOM         1 population_co… 2010-01-01 00:00:00 populat… count   15.5
#>  3 478140 DOM         1 population_co… 2011-01-01 00:00:00 populat… count 3991. 
#>  4 478140 DOM         1 population_co… 2011-01-01 00:00:00 populat… count   13.8
#>  5 478140 DOM         1 population_co… 2012-01-01 00:00:00 populat… count 4068. 
#>  6 478140 DOM         1 population_co… 2012-01-01 00:00:00 populat… count   15.8
#>  7 478140 DOM         1 population_co… 2013-01-01 00:00:00 populat… count 3958. 
#>  8 478140 DOM         1 population_co… 2013-01-01 00:00:00 populat… count   15.2
#>  9 478140 DOM         1 population_co… 2014-01-01 00:00:00 populat… count 3981. 
#> 10 478140 DOM         1 population_co… 2014-01-01 00:00:00 populat… count   15.3
#> # ℹ 12 more rows
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
