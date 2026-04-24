# Global Mangrove Extent Polygon

This resource is part of the publication by Bunting et al. (2018) "The
Global Mangrove Watch—A New 2010 Global Baseline of Mangrove Extent".
The polygons represent the mangrove, which is tropical coastal
vegetation and considered the most significant part of the marine
ecosystem. This resource is available for the selected years in the
period 1996- 2020 from Global Mangrove Watch (GMW), providing geospatial
information about global mangrove extent.

## Usage

``` r
get_gmw(years = c(1996, 2007:2010, 2015:2020))
```

## Source

<https://habitats.oceanplus.org/>

## Arguments

- years:

  A numeric vector of the years for which to make GMW available.

## Value

A function that returns an `sf` footprint object.

## References

Bunting P., Rosenqvist A., Lucas R., Rebelo L-M., Hilarides L., Thomas
N., Hardy A., Itoh T., Shimada M. and Finlayson C.M. (2018). The Global
Mangrove Watch – a New 2010 Global Baseline of Mangrove Extent. Remote
Sensing 10(10): 1669. doi:10.3390/rs10101669.
