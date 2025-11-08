# Calculate precipitation average based on CHELSA

This functions allows to calculate averaged precipitation from the
CHELSA downscaled precipitation layers. Based on user-selected years,
monthly averages of precipitation are calculated.

## Usage

``` r
calc_precipitation_chelsa(years = 1979:2018, engine = "extract")
```

## Arguments

- years:

  A numeric vector indicating the years for which to calculate
  precipitation statistics.

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

## Value

A function that returns an indicator tibble with variable precipitation
and sum of precipitation (in mm/m^2) as value.

## Details

The required resources for this indicator are:

- [chelsa](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/chelsa.md)

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
  get_resources(get_chelsa(years = 2010)) %>%
  calc_indicators(
    calc_precipitation_chelsa(
      years = 2010,
      engine = "extract"
    )
  ) %>%
  portfolio_long()

aoi
#> Simple feature collection with 12 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 12 × 9
#>    WDPAID ISO3  assetid indicator       datetime            variable unit  value
#>     <dbl> <chr>   <int> <chr>           <dttm>              <chr>    <chr> <dbl>
#>  1 478140 DOM         1 precipitation_… 2010-01-01 00:00:00 precipi… mm/m…  52.5
#>  2 478140 DOM         1 precipitation_… 2010-02-01 00:00:00 precipi… mm/m…  10.4
#>  3 478140 DOM         1 precipitation_… 2010-03-01 00:00:00 precipi… mm/m…  32.7
#>  4 478140 DOM         1 precipitation_… 2010-04-01 00:00:00 precipi… mm/m… 104. 
#>  5 478140 DOM         1 precipitation_… 2010-05-01 00:00:00 precipi… mm/m… 218. 
#>  6 478140 DOM         1 precipitation_… 2010-06-01 00:00:00 precipi… mm/m… 142. 
#>  7 478140 DOM         1 precipitation_… 2010-07-01 00:00:00 precipi… mm/m… 191. 
#>  8 478140 DOM         1 precipitation_… 2010-08-01 00:00:00 precipi… mm/m… 153. 
#>  9 478140 DOM         1 precipitation_… 2010-09-01 00:00:00 precipi… mm/m… 161. 
#> 10 478140 DOM         1 precipitation_… 2010-10-01 00:00:00 precipi… mm/m… 116. 
#> 11 478140 DOM         1 precipitation_… 2010-11-01 00:00:00 precipi… mm/m… 209. 
#> 12 478140 DOM         1 precipitation_… 2010-12-01 00:00:00 precipi… mm/m…  39.5
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
