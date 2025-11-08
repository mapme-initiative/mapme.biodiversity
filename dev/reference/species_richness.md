# Species richness based on IUCN raster data

Species richness counts the number of potential species intersecting
with a polygon grouped by the IUCN threat categorization. Note, that
this indicator function requires the manual download of the respective
raster files.

## Usage

``` r
calc_species_richness(engine = "extract", stats = "mean")
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

A function that returns an indicator tibble with IUCN layers with
specified statistics as variable and respective species richness (count)
as value.

## Details

The specific meaning of the species richness indicator depends on the
supplied raster file.

The required resources for this indicator are:

- [iucn](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/iucn.md)

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

iucn_dir <- system.file("res", "iucn", package = "mapme.biodiversity")
sr_rasters <- list.files(iucn_dir, pattern = "*_SR_*", full.names = TRUE)

aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) %>%
  read_sf() %>%
  get_resources(get_iucn(sr_rasters)) %>%
  calc_indicators(calc_species_richness(stats = "median")) %>%
  portfolio_long()

aoi
#> Simple feature collection with 2 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 9
#>   WDPAID ISO3  assetid indicator        datetime            variable unit  value
#>    <dbl> <chr>   <int> <chr>            <dttm>              <chr>    <chr> <dbl>
#> 1 478140 DOM         1 species_richness 2023-01-01 00:00:00 amphibi… count    15
#> 2 478140 DOM         1 species_richness 2023-01-01 00:00:00 birds_t… count    27
#> # ℹ 1 more variable: geom <POLYGON [°]>
# }
```
