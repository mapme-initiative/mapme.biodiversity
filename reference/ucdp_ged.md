# UCDP Georeferenced Event Dataset (UCDP GED)

This resource distributed by the Uppsala Conflict Data Program (UCDP)
constitutes its most disaggregated dataset on individual events of
organized violence. It encodes the different actors involved, is
spatially disaggregated down to village levels anc currently covers the
time period of 1989 to 2021. Older versions of the data set can be
downloaded, but users are recommended to download the latest data set.

## Usage

``` r
get_ucdp_ged(version = "latest")
```

## Source

<https://ucdp.uu.se/downloads/>

## Arguments

- version:

  A character vector specifying the version to download. Defaults to
  "latest".

## Value

A function that returns an `sf` footprint object.

## Details

The following versions are available:

- 5.0

- 17.1

- 17.2

- 18.1

- 19.1

- 20.1

- 21.1

- 22.1

- 23.1

- 24.1

- latest

## References

Davies, Shawn, Therese Pettersson & Magnus Öberg (2022). Organized
violence 1989-2021 and drone warfare. Journal of Peace Research 59(4).
[doi:10.1177/00223433221108428](https://doi.org/10.1177/00223433221108428)
