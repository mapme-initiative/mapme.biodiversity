# Climatologies at High resolution for the Earth Land Surface Areas (CHELSA)

The CHELSA data (Karger et al. 2017) consists of downscaled model output
temperature and precipitation estimates at a horizontal resolution of 30
arc sec. The precipitation algorithm incorporates orographic predictors
including wind fields, valley exposition, and boundary layer height,
with a subsequent bias correction. The spatial resolution is about 1-arc
second (~1km at the equator). This resource makes V2 available.

## Usage

``` r
get_chelsa(years = 1979:2019)
```

## Source

<https://envicloud.wsl.ch/#/?prefix=chelsa/chelsa_V2/GLOBAL/>

## Arguments

- years:

  A numeric vector of the years to make CHELSA monthly precipitation
  layers available for. Must be greater 1979, defaults to
  `c(1979:2019)`.

## Value

A function that returns an `sf` footprint object.

## References

Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza,
R.W., Zimmermann, N.E., Linder, H.P. & Kessler, M. (2021) Climatologies
at high resolution for the earth’s land surface areas. EnviDat.
[doi:10.16904/envidat.228.v2.1](https://doi.org/10.16904/envidat.228.v2.1)

Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza,
R.W., Zimmermann, N.E., Linder, P., Kessler, M. (2017): Climatologies at
high resolution for the Earth land surface areas. Scientific Data. 4
170122.
[doi:10.1038/sdata.2017.122](https://doi.org/10.1038/sdata.2017.122)
