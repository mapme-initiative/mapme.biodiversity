# Calculate Global Surface Water (GSW) Change

The change in water occurrence intensity between the two periods is
derived from homologous pairs of months (i.e. same months containing
valid observations in both periods). The difference in the occurrence of
surface water was calculated for each homologous pair of months. The
average of all of these differences constitutes the Surface Water
Occurrence change intensity. The raster files have integer cell values
between `[0, 200]` where 0 represents surface water loss and 200
represents surface water gain.

## Usage

``` r
calc_gsw_change(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract". Default: "extract".

- stats:

  Aggregation function with which the data are combined. Default:
  "mean".

## Value

A function that returns an indicator tibble with change intensity as
variable and corresponding (unitless) values as value.

## Details

The pixel values are aggregated using method provided via the `stats`
parameter using the specified `engine`.

The required resources for this indicator are:

- [global_surface_water_change](https://mapme-initiative.github.io/mapme.biodiversity/reference/global_surface_water_change.md)

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
  get_resources(get_global_surface_water_change()) %>%
  calc_indicators(
    calc_gsw_change(engine = "extract", stats = "mean")
  ) %>%
  portfolio_long()

aoi
#> Simple feature collection with 1 feature and 10 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -59.84866 ymin: 8.307999 xmax: -59.71 ymax: 8.364002
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 11
#>   WDPAID NAME     DESIG_ENG ISO3  assetid indicator datetime            variable
#>    <dbl> <chr>    <chr>     <chr>   <int> <chr>     <dttm>              <chr>   
#> 1  41057 Shell B… Managed … GUY         1 gsw_chan… 2021-01-01 00:00:00 gsw_cha…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
