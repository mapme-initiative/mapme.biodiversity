# Calculate human footprint statistics

Human footprint data measures the pressure imposed on the natural
environment by different dimensions of human actions. The theoretical
maximum value, representing the highest level of human pressure, is 50.
This routine allows to extract zonal statistics of the human footprint
data.

## Usage

``` r
calc_humanfootprint(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either one
  or multiple inputs as character. Supported statistics are: "mean",
  "median", "sd", "min", "max", "sum" "var".

## Value

A function that returns an indicator tibble the `humanfootprint` as
variable and the associated value (unitless) per year.

## Details

The required resources for this indicator are:

- [humanfootprint_resource](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/humanfootprint_resource.md)

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
  get_resources(get_humanfootprint(years = 2010)) %>%
  calc_indicators(calc_humanfootprint(stats = "median")) %>%
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
#> 1  41057 Shell B… Managed … GUY         1 humanfoo… 2010-01-01 00:00:00 humanfo…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
