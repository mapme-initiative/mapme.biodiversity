# NASA GRACE-based Drought Indicator layer

The resource is published by NASA GRACE Tellus. This data set reflects
on potential drought conditions in the shallow groundwater section
relative to a reference period spanning from 1948 to 2012. It is
available as a global raster with a weekly temporal resolution starting
with the year 2003. The value indicates the wetness percentile of a
given pixel with regard to the reference period.

## Usage

``` r
get_nasa_grace(years = 2003:2022)
```

## Arguments

- years:

  A numeric vector indicating the years for which to make the resource
  available.

## Value

A function that returns an `sf` footprint object.
