# Calculate mangrove extent based on Global Mangrove Watch (GMW)

This function allows to efficiently calculate area of mangrove from
Global Mangrove Watch - World Conservation Monitoring Centre (WCMC) for
polygons. For each polygon, the area of the mangrove (in hectare) for
desired year is returned.

## Usage

``` r
calc_mangroves_area()
```

## Value

A function that returns an indicator tibble with mangroves as variable
and corresponding areas (in ha) as value.

## Details

The required resources for this indicator are:

- [gmw](https://mapme-initiative.github.io/mapme.biodiversity/reference/gmw.md)

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
  get_resources(get_gmw(years = c(1996, 2016))) %>%
  calc_indicators(calc_mangroves_area()) %>%
  portfolio_long()

aoi
#> Simple feature collection with 2 features and 10 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -59.84866 ymin: 8.307999 xmax: -59.71 ymax: 8.364002
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 11
#>   WDPAID NAME     DESIG_ENG ISO3  assetid indicator datetime            variable
#>    <dbl> <chr>    <chr>     <chr>   <int> <chr>     <dttm>              <chr>   
#> 1  41057 Shell B… Managed … GUY         1 mangrove… 1996-01-01 00:00:00 mangrov…
#> 2  41057 Shell B… Managed … GUY         1 mangrove… 2016-01-01 00:00:00 mangrov…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
