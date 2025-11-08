# Calculate Biodiversity Intactness Index

This function calculates the mean biodiversity intactness index for a
region.

## Usage

``` r
calc_biodiversity_intactness_index()
```

## Format

A function that returns an indicator tibble with variable
biodiversity_intactness_index and corresponding values (unitless) as
value.

## Details

The required resources for this indicator are:

- [biodiversity_intactness_index_resource](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/biodiversity_intactness_index_resource.md)

## Examples

``` r
# \dontrun{
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
library(mapme.biodiversity)

outdir <- file.path(tempdir(), "mapme-data")
dir.create(outdir, showWarnings = FALSE)

mapme_options(
  outdir = outdir,
  verbose = FALSE
)

lbii <- system.file("res", "biodiversity_intactness_index", "lbii.asc",
                    package = "mapme.biodiversity")

aoi <- read_sf(
  system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
              package = "mapme.biodiversity"
))
aoi <- get_resources(aoi, get_biodiversity_intactness_index(lbii))
aoi <- calc_indicators(aoi, calc_biodiversity_intactness_index())
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
#> 1  41057 Shell B… Managed … GUY         1 biodiver… 2015-01-01 00:00:00 biodive…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
