# Calculate carbon statistics

These functions allow to calculated statistics based on the harmonized
carbon layers for 2010 and 2018 by Noon et al. (2022).

## Usage

``` r
calc_irr_carbon(
  type = c("total", "soil", "biomass", "all"),
  engine = "extract",
  stats = "mean"
)

calc_man_carbon(
  type = c("total", "soil", "biomass", "all"),
  engine = "extract",
  stats = "mean"
)

calc_vul_carbon(
  type = c("total", "soil", "biomass", "all"),
  engine = "extract",
  stats = "mean"
)
```

## Arguments

- type:

  One of "total", "soil", "biomass", "all". Determines for which data
  layer the statistics are calculated.

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either one
  or multiple inputs as character. Supported statistics are: "mean",
  "median", "sd", "min", "max", "sum", and "var".

## Value

A function that returns an indicator tibble with `(type)_carbon_(stat)`
as variable and the respective statistic (in Mg) as value.

## Details

The required resources for these indicators are:

- [carbon_resources](https://mapme-initiative.github.io/mapme.biodiversity/reference/carbon_resources.md)

Irrecoverable carbon is the amount of carbon that, if lost today, could
not be recovered until 2050. It can be calculated for above- and
below-ground carbon, the total amount of carbon, or for all layers.

Manageable carbon is the amount of carbon that, in principle, is
manageable by human activities, e.g. its release to the atmosphere can
be prevented. It can be calculated for above- and below-ground carbon,
the total amount of carbon, or for all layers.

Vulnerable carbon is the amount of carbon that would be released in a
typical land conversion activity. It can be calculated for above- and
below-ground carbon, the total amount of carbon, or for all layers.

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
  get_resources(
    get_man_carbon(),
    get_vul_carbon(),
    get_irr_carbon()
  ) %>%
  calc_indicators(
    calc_man_carbon(stats = "sum"),
    calc_vul_carbon(stats = "sum"),
    calc_irr_carbon(stats = "sum")
  ) %>%
  portfolio_long()

aoi
#> Simple feature collection with 6 features and 10 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -59.84866 ymin: 8.307999 xmax: -59.71 ymax: 8.364002
#> Geodetic CRS:  WGS 84
#>   WDPAID                       NAME                 DESIG_ENG ISO3 assetid
#> 1  41057 Shell Beach Protected Area Managed Resource Use Area  GUY       1
#> 2  41057 Shell Beach Protected Area Managed Resource Use Area  GUY       1
#> 3  41057 Shell Beach Protected Area Managed Resource Use Area  GUY       1
#> 4  41057 Shell Beach Protected Area Managed Resource Use Area  GUY       1
#> 5  41057 Shell Beach Protected Area Managed Resource Use Area  GUY       1
#> 6  41057 Shell Beach Protected Area Managed Resource Use Area  GUY       1
#>    indicator   datetime             variable unit    value
#> 1 man_carbon 2010-01-01 man_carbon_total_sum   Mg 819413.5
#> 2 man_carbon 2018-01-01 man_carbon_total_sum   Mg 819413.5
#> 3 vul_carbon 2010-01-01 vul_carbon_total_sum   Mg 696439.2
#> 4 vul_carbon 2018-01-01 vul_carbon_total_sum   Mg 696191.6
#> 5 irr_carbon 2010-01-01 irr_carbon_total_sum   Mg 406579.3
#> 6 irr_carbon 2018-01-01 irr_carbon_total_sum   Mg 407012.9
#>                             geom
#> 1 POLYGON ((-59.84334 8.36199...
#> 2 POLYGON ((-59.84334 8.36199...
#> 3 POLYGON ((-59.84334 8.36199...
#> 4 POLYGON ((-59.84334 8.36199...
#> 5 POLYGON ((-59.84334 8.36199...
#> 6 POLYGON ((-59.84334 8.36199...
# }
```
