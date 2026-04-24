# Terrestrial Human Foootprint

This resource is part of the publication by Mu et al. (2022) "A global
record of annual terrestrial Human Footprint dataset from 2000 to 2018".
It is calculated based on 8 variables representing human pressures on
natural ecosystems collected at a yearly cadence between 2000 and 2024
sampled at a 1km spatial resolution. The variables are used are the
expansion of built environments (expressed as percentage of built-up
areas within a grid cell), population density (aggregated at the grid
cell), nighttime lights, crop and pasture lands, roads and railways
(excluding trails and minor roads), and navigable waterways (compares
waterways with nighttime lights dataset). The human footprint was then
calculated based on a weighting scheme proposed by Venter et al. (2016),
assigning each pixel a value between 0 and 50, with 50 representing the
theoretical value of the highest human pressure.

## Usage

``` r
get_humanfootprint(years = 2000:2024)
```

## Source

<https://figshare.com/articles/figure/An_annual_global_terrestrial_Human_Footprint_dataset_from_2000_to_2018/16571064>

## Arguments

- years:

  A numeric vector indicating the years for which to download the human
  footprint data, defaults to `2000:2024`.

## Value

A function that returns an `sf` footprint object.

## Note

It may be required to increase the timeout option to successfully
download theses layers from their source location via e.g.
`options(timeout = 600)`. In case an 403 error occurs, you can create an
account with Figshare and create an personal access token. If set as
`FIGSHARE_PAT` environment variable, it will be used to authenticate.

## References

Mu, H., Li, X., Wen, Y. et al. A global record of annual terrestrial
Human Footprint dataset from 2000 to 2018. Sci Data 9, 176 (2022).
[doi:10.1038/s41597-022-01284-8](https://doi.org/10.1038/s41597-022-01284-8)
