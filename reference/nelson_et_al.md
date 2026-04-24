# Accessibility to Cities layer

This resource is published by Weiss et al. (2018) "A global map of
travel time to cities to assess inequalities in accessibility in 2015"
on journal nature. Accessibility is the ease with which larger cities
can be reached from a certain location. This resource represents the
travel time to major cities in the year 2015. Encoded as minutes,
representing the time needed to reach that particular cell from nearby
city of target population range. The following ranges to nearby cities
are available:

- "5k_10k"

- "10k_20k"

- "20k_50k"

- "50k_100k"

- "100k_200k"

- "200k_500k"

- "500k_1mio"

- "1mio_5mio"

- "50k_50mio"

- "5k_110mio"

- "20k_110mio"

- "5mio_50mio"

## Usage

``` r
get_nelson_et_al(ranges = "20k_50k")
```

## Source

<https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3>

## Arguments

- ranges:

  A character vector indicating one or more ranges to download.

## Value

A function that returns an `sf` footprint object.

## Note

Note, that the 'figshare' server applies a rather restrictive rate limit
thus frequently resulting in opaque error codes (see
<https://github.com/mapme-initiative/mapme.biodiversity/issues/308>).
Please set GDAL configuration options to sensible values in case you are
running into this issue, e.g.:
`Sys.setenv("GDAL_HTTP_MAX_RETRY" = "5", "GDAL_HTTP_RETRY_DELAY" = "15")`.

## References

Weiss, D. J., Nelson, A., Gibson, H. S., Temperley, W., Peedell, S.,
Lieber, A., … & Gething, P. W. (2018). A global map of travel time to
cities to assess inequalities in accessibility in 2015. Nature,
553(7688), 333-336.
