# Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)

This resource is published by Funk et al. (2015) and represents a
quasi-global (50°S-50°S) rainfall estimation at a monthly resolution
starting with the year 1981 to the near-present. It has a spatial
resolution of 0.05°. The data can be used to retrieve information on the
amount of rainfall. Due to the availability of +30 years, anomaly
detection and long-term average analysis is also possible. The routine
will download the complete archive in order to support long-term average
and anomaly calculations with respect to the 1981 - 2010 climate normal
period. Thus no additional arguments need to be specified.

## Usage

``` r
get_chirps(years = 1981:2020)
```

## Source

<https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs/>

## Arguments

- years:

  A numeric vector of the years to download CHIRPS precipitation layers.
  Must be greater 1981, defaults to `c(1981:2020)`.

## Value

A function that returns an `sf` footprint object.

## References

Funk, C., Peterson, P., Landsfeld, M. et al. The climate hazards
infrared precipitation with stations—a new environmental record for
monitoring extremes. Sci Data 2, 150066 (2015).
[doi:10.1038/sdata.2015.66](https://doi.org/10.1038/sdata.2015.66)
