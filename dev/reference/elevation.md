# Calculate elevation statistics

This function allows to calculate elevation statistics for polygons. For
each polygon, the desired statistic(s) are returned.

## Usage

``` r
calc_elevation(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either one
  or multiple inputs as character "mean", "median" or "sd".

## Value

A function that returns an indicator tibble with specified elevation
statistics as variable and corresponding values (in meters) as value.

## Details

The required resources for this indicator are:

- [nasa_srtm](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/nasa_srtm.md)

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
    calc_elevation(engine = "extract", stats = c("mean", "median", "sd", "var"))
  ) %>%
  portfolio_long()

aoi
#> Simple feature collection with 4 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 4 × 9
#>   WDPAID ISO3  assetid indicator datetime            variable       unit   value
#>    <dbl> <chr>   <int> <chr>     <dttm>              <chr>          <chr>  <dbl>
#> 1 478140 DOM         1 elevation 2000-02-01 00:00:00 elevation_mean m      1704.
#> 2 478140 DOM         1 elevation 2000-02-01 00:00:00 elevation_med… m      1702 
#> 3 478140 DOM         1 elevation 2000-02-01 00:00:00 elevation_sd   m       219.
#> 4 478140 DOM         1 elevation 2000-02-01 00:00:00 elevation_var  m     48085.
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
