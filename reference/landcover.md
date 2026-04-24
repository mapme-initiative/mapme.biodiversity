# Calculate area of different landcover classes

The land cover data shows us how much of the region is covered by
forests, rivers, wetlands, barren land, or urban infrastructure thus
allowing the observation of land cover dynamics over a period of time.
This function allows to efficiently calculate area of different
landcover classes for polygons. For each polygon, the area of the
classes in hectare(ha) is returned.

## Usage

``` r
calc_landcover()
```

## Value

A function that returns an indicator tibble with landcover classes as
variables and corresponding areas (in ha) as value.

## Details

The required resources for this indicator are:

- [esalandcover](https://mapme-initiative.github.io/mapme.biodiversity/reference/esalandcover.md)

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
  get_resources(get_esalandcover(years = 2016:2017)) %>%
  calc_indicators(calc_landcover()) %>%
  portfolio_long()

aoi
#> Simple feature collection with 22 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 22 × 9
#>    WDPAID ISO3  assetid indicator datetime            variable      unit   value
#>     <dbl> <chr>   <int> <chr>     <dttm>              <chr>         <chr>  <dbl>
#>  1 478140 DOM         1 landcover 2016-01-01 00:00:00 shrubs        ha    5.06e2
#>  2 478140 DOM         1 landcover 2016-01-01 00:00:00 herbaceous_v… ha    1.84e3
#>  3 478140 DOM         1 landcover 2016-01-01 00:00:00 cropland      ha    1.15e0
#>  4 478140 DOM         1 landcover 2016-01-01 00:00:00 closed_fores… ha    4.65e3
#>  5 478140 DOM         1 landcover 2016-01-01 00:00:00 closed_fores… ha    1.03e1
#>  6 478140 DOM         1 landcover 2016-01-01 00:00:00 closed_fores… ha    4.98e3
#>  7 478140 DOM         1 landcover 2016-01-01 00:00:00 closed_fores… ha    1.46e2
#>  8 478140 DOM         1 landcover 2016-01-01 00:00:00 open_forest_… ha    1.90e3
#>  9 478140 DOM         1 landcover 2016-01-01 00:00:00 open_forest_… ha    8.85e1
#> 10 478140 DOM         1 landcover 2016-01-01 00:00:00 open_forest_… ha    1.49e1
#> # ℹ 12 more rows
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
