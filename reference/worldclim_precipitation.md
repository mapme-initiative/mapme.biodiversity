# Downloads WorldClim Mean Precipitation layer

This resource is published by Fick et al. (2017) "WorldClim 2: new 1-km
spatial resolution climate surfaces for global land areas" and
represents multiple climatic variables from which we will be requiring
minimum temperature, maximum temperature, and mean precipitation layers.
The layers are available to download for the period 1960 - 2024 on
monthly basis from WorldClim.

## Usage

``` r
get_worldclim_precipitation(
  years = 1960:2024,
  resolution = c("2.5m", "5m", "10m")
)
```

## Source

<https://www.worldclim.org/data/index.html>

## Arguments

- years:

  A numeric vector indicating for which years to make the resource
  available.

- resolution:

  A character vector indicating the desired resolution.

## Value

A function that returns an `sf` footprint object.

## Details

This resource represents the average precipitation, layers available to
download for the period 1960 - 2024 on monthly basis from WorldClim.
Encoded as (mm), representing the mean precipitation per output grid
cell.
