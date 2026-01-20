# Calculate areal statistics for IBPES Biomes

This indicator calculates the areal distribution of different biome
classes within an asset based on the IBPES biomes dataset.

## Usage

``` r
calc_ipbes_biomes()
```

## Value

A function that returns an indicator tibble with the biome class as
variable and the respective area (in ha) as value.

## Details

The required resources for this indicator are:

- [ipbes_biomes](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/ipbes_biomes.md)

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
  get_resources(get_ipbes_biomes()) %>%
  calc_indicators(calc_ipbes_biomes()) %>%
  portfolio_long()
#> Error in st_sf(x, ..., agr = agr, sf_column_name = sf_column_name) : 
#>   no simple features geometry column present
#> Error in .check_footprints(resource, resource_name): Download for resource ipbes_biomes failed.
#> Returning unmodified portfolio.

aoi
#> Error: object 'aoi' not found
# }
```
