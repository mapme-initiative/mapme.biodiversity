# Calculate Monthly Burned Area based on MODIS (MCD64A1)

Calculates Monthly Burned Area based on the Terra and Aqua combined
MCD64A1 Version 6.1. which s a monthly, global gridded 500 meter (m)
product containing per-pixel burned-area information.

## Usage

``` r
calc_burned_area(engine = "extract")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

## Value

A function that returns an indicator tibble with variable burned area
and corresponding area (in ha) as values.

## Details

The required resources for this indicator are:

- [mcd64a1](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mcd64a1.md)

## References

Giglio, L., C. Justice, L. Boschetti, D. Roy. MODIS/Terra+Aqua Burned
Area Monthly L3 Global 500m SIN Grid V061. 2021, distributed by NASA
EOSDIS Land Processes Distributed Active Archive Center.
[doi:10.5067/MODIS/MCD64A1.061](https://doi.org/10.5067/MODIS/MCD64A1.061)

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
  get_resources(get_mcd64a1(years = 2010)) %>%
  calc_indicators(calc_burned_area(engine = "extract")) %>%
  portfolio_long()

aoi
#> Simple feature collection with 12 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 12 × 9
#>    WDPAID ISO3  assetid indicator   datetime            variable unit      value
#>     <dbl> <chr>   <int> <chr>       <dttm>              <chr>    <chr>     <dbl>
#>  1 478140 DOM         1 burned_area 2010-12-01 00:00:00 burned_… ha    2.91e-318
#>  2 478140 DOM         1 burned_area 2010-11-01 00:00:00 burned_… ha    2.91e-318
#>  3 478140 DOM         1 burned_area 2010-10-01 00:00:00 burned_… ha    2.97e-318
#>  4 478140 DOM         1 burned_area 2010-09-01 00:00:00 burned_… ha    2.97e-318
#>  5 478140 DOM         1 burned_area 2010-08-01 00:00:00 burned_… ha    2.91e-318
#>  6 478140 DOM         1 burned_area 2010-07-01 00:00:00 burned_… ha    2.91e-318
#>  7 478140 DOM         1 burned_area 2010-06-01 00:00:00 burned_… ha    2.92e-318
#>  8 478140 DOM         1 burned_area 2010-05-01 00:00:00 burned_… ha    3.67e-318
#>  9 478140 DOM         1 burned_area 2010-04-01 00:00:00 burned_… ha    3.54e-318
#> 10 478140 DOM         1 burned_area 2010-03-01 00:00:00 burned_… ha    4.28e+  1
#> 11 478140 DOM         1 burned_area 2010-02-01 00:00:00 burned_… ha    2.91e-318
#> 12 478140 DOM         1 burned_area 2010-01-01 00:00:00 burned_… ha    2.91e-318
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
