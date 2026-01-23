# Calculate areal statistics for IBPES Biomes

This indicator calculates the areal distribution of different biome
classes within an asset based on the IBPES biomes dataset.

## Usage

``` r
calc_ipbes_biomes()
```

## Value

A function that returns an indicator tibble with the biome class as
variable and the respective area (in ha) as value.

## Details

The required resources for this indicator are:

- [ipbes_biomes](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/ipbes_biomes.md)

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

aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
  package = "mapme.biodiversity"
) %>%
  read_sf() %>%
  get_resources(get_ipbes_biomes()) %>%
  calc_indicators(calc_ipbes_biomes()) %>%
  portfolio_long()
#> Warning: GDAL Message 1: HTTP response code on https://zenodo.org/records/3975694/files/IPBES_UoA_biomes_JK.AUX: 504

aoi
#> Simple feature collection with 2 features and 10 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -59.84866 ymin: 8.307999 xmax: -59.71 ymax: 8.364002
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 11
#>   WDPAID NAME     DESIG_ENG ISO3  assetid indicator datetime            variable
#>    <dbl> <chr>    <chr>     <chr>   <int> <chr>     <dttm>              <chr>   
#> 1  41057 Shell B… Managed … GUY         1 ipbes_bi… 2019-01-01 00:00:00 tropica…
#> 2  41057 Shell B… Managed … GUY         1 ipbes_bi… 2019-01-01 00:00:00 shelf_e…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
