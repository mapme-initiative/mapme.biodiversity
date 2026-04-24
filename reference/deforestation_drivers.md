# Calculate deforestation drivers

This function extracts areal statistics for the drivers of deforestation
based on the data source produced by Fritz et al (2022).

## Usage

``` r
calc_deforestation_drivers()
```

## Value

A function that returns an indicator tibble with deforestation drivers
as variable and corresponding area (in ha) as value.

## Details

The required resource for this indicator is:

- [fritz_et_al](https://mapme-initiative.github.io/mapme.biodiversity/reference/fritz_et_al.md)

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
  get_resources(get_fritz_et_al(resolution = 100)) %>%
  calc_indicators(calc_deforestation_drivers()) %>%
  portfolio_long()

aoi
#> Simple feature collection with 10 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 10 × 9
#>    WDPAID ISO3  assetid indicator      datetime            variable unit   value
#>     <dbl> <chr>   <int> <chr>          <dttm>              <chr>    <chr>  <dbl>
#>  1 478140 DOM         1 deforestation… 2008-01-01 00:00:00 commerc… ha        0 
#>  2 478140 DOM         1 deforestation… 2008-01-01 00:00:00 commerc… ha        0 
#>  3 478140 DOM         1 deforestation… 2008-01-01 00:00:00 managed… ha        0 
#>  4 478140 DOM         1 deforestation… 2008-01-01 00:00:00 mining   ha        0 
#>  5 478140 DOM         1 deforestation… 2008-01-01 00:00:00 natural… ha        0 
#>  6 478140 DOM         1 deforestation… 2008-01-01 00:00:00 pasture  ha        0 
#>  7 478140 DOM         1 deforestation… 2008-01-01 00:00:00 roads    ha        0 
#>  8 478140 DOM         1 deforestation… 2008-01-01 00:00:00 wildfire ha        0 
#>  9 478140 DOM         1 deforestation… 2008-01-01 00:00:00 other_s… ha    16809.
#> 10 478140 DOM         1 deforestation… 2008-01-01 00:00:00 shiftin… ha        0 
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
