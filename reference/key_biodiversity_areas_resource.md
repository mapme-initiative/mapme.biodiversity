# Key Biodiversity Areas

This resource contains outlines of key biodiversity areas, which are
areas representing sites with specific importance for nature
conservation.

## Usage

``` r
get_key_biodiversity_areas(path = NULL)
```

## Source

<https://www.keybiodiversityareas.org/request-gis-data>

## Arguments

- path:

  A character vector to the key biodiversity areas GPKG file. Note, that
  the file has to be downloaded manually.

## Value

A function that returns an `sf` footprints object.

## Details

To use this data in mapme workflows, you will have to manually download
the global data set and point towards its file path on your local
machine. Please find the available data under the source link given
below.

## References

BirdLife International (2024). The World Database of Key Biodiversity
Areas. Developed by the KBA Partnership: BirdLife International,
International Union for the Conservation of Nature, Amphibian Survival
Alliance, Conservation International, Critical Ecosystem Partnership
Fund, Global Environment Facility, Re:wild, NatureServe, Rainforest
Trust, Royal Society for the Protection of Birds, Wildlife Conservation
Society and World Wildlife Fund. Available at
www.keybiodiversityareas.org.
