# Calculate treecover statistics

This functions allows to efficiently calculate treecover statistics for
polygons. For each year in the analysis timeframe, the forest losses in
preceding and the current years are subtracted from the treecover in the
year 2000 and actual treecover figures within the polygon are returned.

## Usage

``` r
calc_treecover_area(years = 2000:2024, min_size = 10, min_cover = 35)
```

## Arguments

- years:

  A numeric vector with the years for which to calculate treecover area.

- min_size:

  The minimum size of a forest patch to be considered as forest in ha.

- min_cover:

  The minimum cover percentage per pixel to be considered as forest.

## Value

A function that returns an indicator tibble with variable treecover and
corresponding area (in ha) as value.

If the
[gfw_treecover](https://mapme-initiative.github.io/mapme.biodiversity/reference/gfw_treecover.md)
resource for an asset contains only zeros (no trees at all, e.g. big
water body like an ocean or a sea), the `NULL` value is returned instead
of the indicator tibble. If it doesn't contain any pixels with value \>=
`min_cover`, indicator tibble reports area of zero hectares.

## Details

The required resources for this indicator are:

- [gfw_treecover](https://mapme-initiative.github.io/mapme.biodiversity/reference/gfw_treecover.md)

- [gfw_lossyear](https://mapme-initiative.github.io/mapme.biodiversity/reference/gfw_lossyear.md)

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
  get_resources(
    get_gfw_treecover(version = "GFC-2024-v1.12"),
    get_gfw_lossyear(version = "GFC-2024-v1.12")
  ) %>%
  calc_indicators(calc_treecover_area(years = 2016:2017, min_size = 1, min_cover = 30)) %>%
  portfolio_long()

aoi
#> Simple feature collection with 2 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 9
#>   WDPAID ISO3  assetid indicator      datetime            variable  unit   value
#>    <dbl> <chr>   <int> <chr>          <dttm>              <chr>     <chr>  <dbl>
#> 1 478140 DOM         1 treecover_area 2016-01-01 00:00:00 treecover ha    11964.
#> 2 478140 DOM         1 treecover_area 2017-01-01 00:00:00 treecover ha    11893.
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
