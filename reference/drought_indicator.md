# Calculate drought indicator statistics

This function allows to efficiently calculate the relative wetness in
the shallow groundwater section with regard to the the 1948-2012
reference period. The values represent the wetness percentile a given
area achieves at a given point in time in regard to the reference
period. For each polygon, the desired statistic/s (mean, median or sd)
is/are returned.

## Usage

``` r
calc_drought_indicator(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either one
  or multiple inputs as character "mean", "median" or "sd".

## Value

A function that returns an indicator tibble with specified drought
indicator statistics as variable and corresponding values as value.

## Details

The required resources for this indicator are:

- [nasa_grace](https://mapme-initiative.github.io/mapme.biodiversity/reference/nasa_grace.md)

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
  get_resources(get_nasa_grace(years = 2022)) %>%
  calc_indicators(
    calc_drought_indicator(
      engine = "extract",
      stats = c("mean", "median")
    )
  ) %>%
  portfolio_long()

aoi
#> Simple feature collection with 40 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 40 × 9
#>    WDPAID ISO3  assetid indicator       datetime            variable unit  value
#>     <dbl> <chr>   <int> <chr>           <dttm>              <chr>    <chr> <dbl>
#>  1 478140 DOM         1 drought_indica… 2022-01-03 00:00:00 wetness… perc…  57.5
#>  2 478140 DOM         1 drought_indica… 2022-01-03 00:00:00 wetness… perc…  57.5
#>  3 478140 DOM         1 drought_indica… 2022-01-10 00:00:00 wetness… perc…  55.5
#>  4 478140 DOM         1 drought_indica… 2022-01-10 00:00:00 wetness… perc…  55.5
#>  5 478140 DOM         1 drought_indica… 2022-01-17 00:00:00 wetness… perc…  54  
#>  6 478140 DOM         1 drought_indica… 2022-01-17 00:00:00 wetness… perc…  54  
#>  7 478140 DOM         1 drought_indica… 2022-01-24 00:00:00 wetness… perc…  53  
#>  8 478140 DOM         1 drought_indica… 2022-01-24 00:00:00 wetness… perc…  53  
#>  9 478140 DOM         1 drought_indica… 2022-01-31 00:00:00 wetness… perc…  43.5
#> 10 478140 DOM         1 drought_indica… 2022-01-31 00:00:00 wetness… perc…  43.5
#> # ℹ 30 more rows
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
