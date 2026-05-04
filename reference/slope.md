# Calculate slope statistics

This function allows to calculate slope statistics for polygons. For
each polygon, the desired statistic(s) are returned.

## Usage

``` r
calc_slope(engine = "exactextract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing function from either one of "zonal",
  "extract" or "exactextract" as a character string.

- stats:

  Function to be applied to compute statistics for polygons. Accepts
  either a single string or a vector of strings, such as "mean",
  "median", "sd", "min", "max", "sum", or "var".

## Value

A function that returns an indicator tibble with specified slope
statistics as variables and corresponding values (in degrees).

## Details

The required resource for this indicator is:

- [nasa_srtm](https://mapme-initiative.github.io/mapme.biodiversity/reference/nasa_srtm.md)

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
  get_resources(get_nasa_srtm()) %>%
  calc_indicators(
    calc_slope(stats = c("mean", "median", "sd", "var"), engine = "extract")
  ) %>%
  portfolio_long()
#> Resource 'nasa_srtm' is already available.

aoi
#> Simple feature collection with 4 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 4 × 9
#>   WDPAID ISO3  assetid indicator datetime            variable     unit    value
#>    <dbl> <chr>   <int> <chr>     <dttm>              <chr>        <chr>   <dbl>
#> 1 478140 DOM         1 slope     2000-02-01 00:00:00 slope_mean   degrees 17.8 
#> 2 478140 DOM         1 slope     2000-02-01 00:00:00 slope_median degrees 17.0 
#> 3 478140 DOM         1 slope     2000-02-01 00:00:00 slope_sd     degrees  9.93
#> 4 478140 DOM         1 slope     2000-02-01 00:00:00 slope_var    degrees 98.6 
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
