# Calculate treeloss statistics

This functions allows to efficiently calculate the treecover and
emissions indicators in a single function call together. Since most of
the pre-processing operations for treecover and emissions are the same,
it is more efficient to calculate them in one run if users are actually
interested in both statistics. Otherwise users are advised to use the
respective single indicator functions.

## Usage

``` r
calc_treecover_area_and_emissions(
  years = 2000:2024,
  min_size = 10,
  min_cover = 35
)
```

## Arguments

- years:

  A numeric vector with the years for which to calculate treecover area
  and emissions.

- min_size:

  The minimum size of a forest patch in ha.

- min_cover:

  The minimum threshold of stand density for a pixel to be considered
  forest in the year 2000.

## Value

A function that returns an indicator tibble with variables treecover and
emissions and corresponding values (in ha and Mg) as value.

## Details

The required resources for this indicator are:

- [gfw_treecover](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gfw_treecover.md)

- [gfw_lossyear](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gfw_lossyear.md)

- [gfw_emissions](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gfw_emissions.md)

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
    get_gfw_lossyear(version = "GFC-2024-v1.12"),
    get_gfw_emissions()
  ) %>%
  calc_indicators(
    calc_treecover_area_and_emissions(years = 2016:2017, min_size = 1, min_cover = 30)
  ) %>%
  portfolio_long()
#> Resource 'gfw_treecover' is already available.
#> Resource 'gfw_lossyear' is already available.

aoi
#> Simple feature collection with 4 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 4 × 9
#>   WDPAID ISO3  assetid indicator       datetime            variable unit   value
#>    <dbl> <chr>   <int> <chr>           <dttm>              <chr>    <chr>  <dbl>
#> 1 478140 DOM         1 treecover_area… 2016-01-01 00:00:00 emissio… Mg     4296.
#> 2 478140 DOM         1 treecover_area… 2016-01-01 00:00:00 treecov… ha    11964.
#> 3 478140 DOM         1 treecover_area… 2017-01-01 00:00:00 emissio… Mg     4970.
#> 4 478140 DOM         1 treecover_area… 2017-01-01 00:00:00 treecov… ha    11893.
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
