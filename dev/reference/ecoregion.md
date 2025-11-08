# Calculate terrestrial ecoregions statistics (TEOW) based on WWF

This function allows to efficiently retrieve the name of the ecoregions
and compute the corresponding area from Terrestrial Ecoregions of the
World (TEOW) - World Wildlife Fund (WWF) for polygons. For each polygon,
the name and area of the ecoregions (in hectare) is returned. The
required resources for this indicator are:

- [teow](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/teow.md)

## Usage

``` r
calc_ecoregion()
```

## Value

A function that returns an indicator tibble with ecoregion type as
variable and corresponding area (in ha) as value.

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
  get_resources(get_teow()) %>%
  calc_indicators(calc_ecoregion()) %>%
  portfolio_long()
#> Error in get_teow(): Expecting path to point towards an existing '.zip' file.

aoi
#> Error: object 'aoi' not found
# }
```
