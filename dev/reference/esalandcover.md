# ESA Copernicus Global Land Cover layer

This 100 meter spatial resolution land cover resource is published by
Buchhorn et al. (2020) "Copernicus Global Land Cover Layers—Collection
2". The resource represents the actual surface cover of ground available
annually for the period 2015 to 2019. The cell values range from 0 to
200, representing total of 23 discrete classifications from ESA.

## Usage

``` r
get_esalandcover(years = 2015L:2019L)
```

## Source

<https://zenodo.org/records/3939050>

## Arguments

- years:

  A numeric vector indicating the years for which to make the resource
  available.

## Value

A function that returns an `sf` footprint object.

## Details

This function mimics the behavior of the original `get_esalandcover()`
function by downloading the parts of the global raster that correspond
to the 20° x 20° tiles used previously. The files are named the same as
before. This allows other code that uses this resource to remain
unchanged.

## References

Buchhorn, M., Lesiv, M., Tsendbazar, N.-E., Herold, M., Bertels, L., &
Smets, B. (2020). Copernicus Global Land Cover Layers - Collection 2.
Remote Sensing, 12(6), 1044.
[doi:10.3390/rs12061044](https://doi.org/10.3390/rs12061044)
