# Calculate Global Surface Water (GSW) Transitions

GSW transition data contains information about the type of surface water
change for each pixel. The raster files have integer cell values between
`[0, 10]` that code for different transition classes:

## Usage

``` r
calc_gsw_transitions()
```

## Value

A function that returns an indicator tibble with transition classes as
variable and corresponding areas (in ha) as value.

## Details

|       |                       |
|-------|-----------------------|
| Value | Transition Class      |
| 1     | Permanent             |
| 2     | New Permanent         |
| 3     | Lost Permanent        |
| 4     | Seasonal              |
| 5     | New Seasonal          |
| 6     | Lost Seasonal         |
| 7     | Seasonal to Permanent |
| 8     | Permanent to Seasonal |
| 9     | Ephemeral Permanent   |
| 10    | Ephemeral Seasonal    |

To aggregate, we sum up the area of each transition class for a given
region.

The required resources for this indicator are:

- [global_surface_water_transitions](https://mapme-initiative.github.io/mapme.biodiversity/reference/global_surface_water_transitions.md)

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
  get_resources(get_global_surface_water_transitions()) %>%
  calc_indicators(calc_gsw_transitions()) %>%
  portfolio_long()

aoi
#> Simple feature collection with 9 features and 10 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -59.84866 ymin: 8.307999 xmax: -59.71 ymax: 8.364002
#> Geodetic CRS:  WGS 84
#> # A tibble: 9 × 11
#>   WDPAID NAME     DESIG_ENG ISO3  assetid indicator datetime            variable
#>    <dbl> <chr>    <chr>     <chr>   <int> <chr>     <dttm>              <chr>   
#> 1  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_per…
#> 2  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_new…
#> 3  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_los…
#> 4  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_sea…
#> 5  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_new…
#> 6  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_sea…
#> 7  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_per…
#> 8  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_eph…
#> 9  41057 Shell B… Managed … GUY         1 gsw_tran… 2021-01-01 00:00:00 gsw_eph…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
