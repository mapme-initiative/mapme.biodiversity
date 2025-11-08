# IUCN Red List of Threatened Species

This resource is part of the spatial data set Red List of Threatened
Species released by IUCN. It is free to use under a non-commercial
licence. For commercial uses, a request has to be sent to Integrated
Biodiversity Assessment Tool (IBAT).

## Usage

``` r
get_iucn(paths = NULL)
```

## Source

<https://www.iucnredlist.org/resources/other-spatial-downloads>

## Arguments

- paths:

  A character vector to the respective species range files in GTiff
  format. Note, that theses files have to be downloaded manually.

## Value

A function that returns an `sf` footprint object.

## Details

To use this data in mapme workflows, you will have to manually download
the global data set and point towards the file path on your local
machine. Please find the available data under the source link given
below.

## References

IUCN (2024). The IUCN Red List of Threatened Species.
<https://www.iucnredlist.org>
