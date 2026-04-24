# Calculate Global Surface Water (GSW) Recurrence

Water Recurrence is a measurement of the degree of variability in the
presence of water from year to year. It describes the frequency with
which water returned to a particular location from one year to another,
and is expressed as a percentage. The raster files have integer cell
values between `[0, 100]`, where 100 represents that water reoccurs
predictably every year, whereas lower values indicate that water only
occurs episodically.

## Usage

``` r
calc_gsw_recurrence(engine = "extract", min_recurrence = NULL)
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract". Default: "extract".

- min_recurrence:

  Threshold to define which pixels count towards the GSW recurrence area
  `[0, 100]`.

## Value

A function that returns an indicator tibble with recurrence as variable
and the corresponding area (in ha) as value.

## Details

The raw data values are aggregated based on a provided threshold
parameter `min_recurrence`, the function returns the area covered by
values greater or equal than this threshold.

The required resources for this indicator are:

- [global_surface_water_recurrence](https://mapme-initiative.github.io/mapme.biodiversity/reference/global_surface_water_recurrence.md)

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
  get_resources(get_global_surface_water_recurrence()) %>%
  calc_indicators(
    calc_gsw_recurrence(engine = "extract", min_recurrence = 10)
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
#> 1  41057 Shell B… Managed … GUY         1 gsw_recu… 2021-01-01 00:00:00 gsw_rec…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
