# Population Count layer for year 2000-2020

This resource is published by open spatial demographic data and research
organization called WorldPop. This resource represents the population
count, 1 km spatial resolution layers available to download from the
year 2000 to 2020. The dataset is called as WorldPop Unconstrained
Global Mosaics. The encoded cell value represents the total number of
people in that particular grid cell.

## Usage

``` r
get_worldpop(years = 2000)
```

## Source

<https://www.worldpop.org/>

## Arguments

- years:

  A numeric vector indicating the years for which to make the resource
  available.

## Value

A function that returns an `sf` footprint object.

## Details

It may be required to increase the timeout option to successfully
download theses WorldPop layers from their source location via e.g.
`options(timeout = 600)`.
