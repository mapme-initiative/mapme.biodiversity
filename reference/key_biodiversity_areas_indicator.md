# Calculate Key Biodiversity Areas

This function calculates the total area of key biodiversity areas for a
given input polygon.

## Usage

``` r
calc_key_biodiversity_area()
```

## Format

A function returning an indicator tibble with `key_biodiversity_area` as
variable and the total overlap area (in ha) as value.

## Details

The required resources for this indicator are:

- [key_biodiversity_areas_resource](https://mapme-initiative.github.io/mapme.biodiversity/reference/key_biodiversity_areas_resource.md)

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

aoi <- read_sf(
  system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
              package = "mapme.biodiversity"
))
kbas <- system.file("res", "key_biodiversity_areas", "kbas.gpkg",
                    package = "mapme.biodiversity")
aoi <- get_resources(aoi, get_key_biodiversity_areas(kbas))
aoi <- calc_indicators(aoi, calc_key_biodiversity_area())
aoi <- portfolio_long(aoi)

aoi
#> Simple feature collection with 1 feature and 10 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -59.84866 ymin: 8.307999 xmax: -59.71 ymax: 8.364002
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 11
#>   WDPAID NAME     DESIG_ENG ISO3  assetid indicator datetime            variable
#>    <dbl> <chr>    <chr>     <chr>   <int> <chr>     <dttm>              <chr>   
#> 1  41057 Shell B… Managed … GUY         1 key_biod… 2024-01-01 00:00:00 key_bio…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
