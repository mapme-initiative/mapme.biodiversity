# Calculate Terrain Ruggedness Index (TRI) statistics

Terrain Ruggedness Index is a measurement developed by Riley, et al.
(1999). The elevation difference between the centre pixel and its eight
immediate pixels are squared and then averaged and its square root is
taken to get the TRI value. This function allows to calculate terrain
ruggedness index (tri) statistics for polygons. For each polygon, the
desired statistic(s) are returned.

## Usage

``` r
calc_tri(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either
  single or multiple inputs as character. Supported statistics are:
  "mean", "median", "sd", "min", "max", "sum" "var".

## Value

A function that returns an indicator tibble with tri as variable and the
respective statistic as value.

## Details

The range of index values and corresponding meaning:

- 0-80 m - level surface

- 81-116 m - nearly level surface

- 117-161 m - slightly rugged surface

- 162-239 m - intermediately rugged surface

- 240-497 m - moderately rugged surface

- 498-958 m - highly rugged surface

- 959-4367 m extremely rugged surface

The required resources for this indicator are:

- [nasa_srtm](https://mapme-initiative.github.io/mapme.biodiversity/reference/nasa_srtm.md)

## References

Riley, S. J., DeGloria, S. D., & Elliot, R. (1999). Index that
quantifies topographic heterogeneity. Intermountain Journal of Sciences,
5(1-4), 23-27.

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
    calc_tri(stats = c("mean", "median", "sd", "var"), engine = "extract")
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
#>   WDPAID ISO3  assetid indicator datetime            variable   unit  value
#>    <dbl> <chr>   <int> <chr>     <dttm>              <chr>      <chr> <dbl>
#> 1 478140 DOM         1 tri       2000-02-01 00:00:00 tri_mean   m      33.3
#> 2 478140 DOM         1 tri       2000-02-01 00:00:00 tri_median m      30.8
#> 3 478140 DOM         1 tri       2000-02-01 00:00:00 tri_sd     m      18.7
#> 4 478140 DOM         1 tri       2000-02-01 00:00:00 tri_var    m     349. 
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
