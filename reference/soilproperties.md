# Calculate Zonal Soil Properties

This indicator allows the extraction of zonal statistics for resource
layers previously downloaded from SoilGrids, thus in total supporting
the calculation of zonal statistics for 10 different soil properties at
6 different depths for a total of 4 different model outputs (stat).
Zonal statistics will be calculated for all SoilGrid layers that have
been previously made available vie
[`get_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/reference/mapme.md).

## Usage

``` r
calc_soilproperties(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either
  single or multiple inputs as character. Supported statistics are:
  "mean", "median", "sd", "min", "max", "sum" "var".

## Value

A function that returns an indicator tibble with soilgrid layers and
statistics as variables and the corresponding statistics as value.

## Details

The required resource for this indicator is:

- [soilgrids](https://mapme-initiative.github.io/mapme.biodiversity/reference/soilgrids.md)

## Examples

``` r
if (FALSE) {
  library(sf)
  library(mapme.biodiversity)

  mapme_options(
    outdir = NULL,
    verbose = FALSE
  )

  aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
    package = "mapme.biodiversity"
  ) %>%
    read_sf() %>%
    get_resources(
      get_soilgrids(
        layers = "clay",
        depths = "0-5cm",
        stats = "mean"
      )
    ) %>%
    calc_indicators(
      calc_soilproperties(engine = "extract", stats = c("mean", "median"))
    ) %>%
    portfolio_long()

  aoi
}
```
