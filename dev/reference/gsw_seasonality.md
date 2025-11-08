# Calculate Global Surface Water (GSW) Seasonality

GSW seasonality describes the intra-annual distribution of surface water
for each pixel. The raster files have integer cell values between
`[0, 12]`, indicating how many months per year the pixel was classified
as water.

## Usage

``` r
calc_gsw_seasonality()
```

## Value

A function that returns an indicator tibble with seasonality categories
as variables and corresponding areas (in ha) as value.

## Details

The pixel values are aggregated using method provided via the `stats`
parameter.

The required resources for this indicator are:

- [global_surface_water_seasonality](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/global_surface_water_seasonality.md)

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
  get_resources(get_global_surface_water_seasonality()) %>%
  calc_indicators(calc_gsw_seasonality()) %>%
  portfolio_long()

aoi
#> Simple feature collection with 13 features and 10 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -59.84866 ymin: 8.307999 xmax: -59.71 ymax: 8.364002
#> Geodetic CRS:  WGS 84
#> # A tibble: 13 × 11
#>    WDPAID NAME    DESIG_ENG ISO3  assetid indicator datetime            variable
#>     <dbl> <chr>   <chr>     <chr>   <int> <chr>     <dttm>              <chr>   
#>  1  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#>  2  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#>  3  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#>  4  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#>  5  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#>  6  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#>  7  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#>  8  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#>  9  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#> 10  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#> 11  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#> 12  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#> 13  41057 Shell … Managed … GUY         1 gsw_seas… 2021-01-01 00:00:00 gsw_sea…
#> # ℹ 3 more variables: unit <chr>, value <dbl>, geom <POLYGON [°]>
# }
```
