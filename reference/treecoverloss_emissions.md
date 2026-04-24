# Calculate emission statistics

This functions allows to efficiently calculate emission statistics for
areas of interest. For each year in the analysis timeframe, the forest
losses from Hansen et al. (2013) are overlayed with the respective
emission layer from Harris et al. (2021) and area-wise emission
statistics are calculated for each year.

## Usage

``` r
calc_treecoverloss_emissions(years = 2000:2024, min_size = 10, min_cover = 35)
```

## Arguments

- years:

  A numeric vector with the years for which to calculate emissions
  caused by treecover loss.

- min_size:

  The minimum size of a forest patch in ha.

- min_cover:

  The minimum threshold of stand density for a pixel to be considered
  forest in the year 2000.

## Value

A function that returns an indicator tibble with emissions as variable
and emitted CO2 equivalent (in Mg) as value.

## Details

The required resources for this indicator are:

- [gfw_treecover](https://mapme-initiative.github.io/mapme.biodiversity/reference/gfw_treecover.md)

- [gfw_lossyear](https://mapme-initiative.github.io/mapme.biodiversity/reference/gfw_lossyear.md)

- [gfw_emissions](https://mapme-initiative.github.io/mapme.biodiversity/reference/gfw_emissions.md)

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
    calc_treecoverloss_emissions(years = 2016:2017, min_size = 1, min_cover = 30)
  ) %>%
  portfolio_long()
#> Resource 'gfw_treecover' is already available.
#> Resource 'gfw_lossyear' is already available.
#> Resource 'gfw_emissions' is already available.

aoi
#> Simple feature collection with 2 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 9
#>   WDPAID ISO3  assetid indicator        datetime            variable unit  value
#>    <dbl> <chr>   <int> <chr>            <dttm>              <chr>    <chr> <dbl>
#> 1 478140 DOM         1 treecoverloss_e… 2016-01-01 00:00:00 emissio… Mg    4296.
#> 2 478140 DOM         1 treecoverloss_e… 2017-01-01 00:00:00 emissio… Mg    4970.
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
