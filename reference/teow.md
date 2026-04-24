# Terrestrial Ecoregions of the World (TEOW) Polygon

This resource is part of the publication by Olson et al. (2004)
"Terrestrial Ecosystems of the World (TEOW) from WWF-US (Olson)". It
depicts 867 terrestrial ecoregions around the world classified into 14
different terrestrial biomes such as forests, grasslands, or deserts.
The polygons represent the ecoregions, defined as relatively large units
of land or inland water sharing a large majority of biodiversity. The
datasets is made available from World Wildlife Fund (WWF) for the year
2001.

## Usage

``` r
get_teow(path = NULL)
```

## Source

<https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/6kcchn7e3u_official_teow.zip>

## Arguments

- path:

  A character vector to the Terrestrial Ecoregions of the World (TEOW)
  zip file. Note, that the file has to be downloaded manually.

## Value

A function that returns an `sf` footprint object.

## References

Olson, D. M., Dinerstein, E., Wikramanayake, E. D., Burgess, N. D.,
Powell, G. V. N., Underwood, E. C., D’Amico, J. A., Itoua, I., Strand,
H. E., Morrison, J. C., Loucks, C. J., Allnutt, T. F., Ricketts, T. H.,
Kura, Y., Lamoreux, J. F., Wettengel, W. W., Hedao, P., Kassem, K. R.
2001. Terrestrial ecoregions of the world: a new map of life on Earth.
Bioscience 51(11):933-938.
[doi:10.1641/0006-3568(2001)051\[0933:TEOTWA\]2.0.CO;2](https://doi.org/10.1641/0006-3568%282001%29051%5B0933%3ATEOTWA%5D2.0.CO%3B2)
