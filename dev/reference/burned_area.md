# Calculate Monthly Burned Area based on MODIS (MCD64A1)

Calculates Monthly Burned Area based on the Terra and Aqua combined
MCD64A1 Version 6.1. which s a monthly, global gridded 500 meter (m)
product containing per-pixel burned-area information.

## Usage

``` r
calc_burned_area(engine = "extract")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

## Value

A function that returns an indicator tibble with variable burned area
and corresponding area (in ha) as values.

## Details

The required resources for this indicator are:

- [mcd64a1](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mcd64a1.md)

## References

Giglio, L., C. Justice, L. Boschetti, D. Roy. MODIS/Terra+Aqua Burned
Area Monthly L3 Global 500m SIN Grid V061. 2021, distributed by NASA
EOSDIS Land Processes Distributed Active Archive Center.
[doi:10.5067/MODIS/MCD64A1.061](https://doi.org/10.5067/MODIS/MCD64A1.061)

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
  get_resources(get_mcd64a1(years = 2010)) %>%
  calc_indicators(calc_burned_area(engine = "extract")) %>%
  portfolio_long()
#> Error : HTTP status '404'. The given location does not exist or cannot be read
#> Error in .check_footprints(resource, resource_name): Download for resource mcd64a1 failed.
#> Returning unmodified portfolio.

aoi
#> Error: object 'aoi' not found
# }
```
