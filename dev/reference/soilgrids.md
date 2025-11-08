# SoilGrids data layers

SoilGrids is a project combining global observation data with machine
learning to map the spatial distribution of soil properties across the
globe. It is produced at a spatial resolution of 250 meters and each
parameter is mapped at different depths. In order to be able to assess
prediction uncertainty, besides the mean and median prediction, the 0.05
and 0.95 percentile predictions are available. The following parameters
are available:

- bdod:

  Bulk density of the fine earth fraction (kg/dm3)

- cec:

  Cation Exchange Capacity of the soil (cmol(c)/kg)

- cfvo:

  Volumetric fraction of coarse fragments \> 2 mm (cm3/100cm3 (volPerc))

- clay:

  Proportion of clay particles \< 0.002 mm in the fine earth fraction
  (g/100g)

- nitrogen:

  Total nitrogen (g/kg)

- phh2o:

  Soil pH (pH)

- sand:

  Proportion of sand particles \> 0.05 mm in the fine earth fraction
  (g/100g)

- silt:

  Proportion of silt particles \>= 0.002 mm and \<= 0.05 mm in the fine
  earth fraction (g/100g)

- soc:

  Soil organic carbon content in the fine earth fraction (g/kg)

- ocd:

  Organic carbon density (kg/m3)

- ocs:

  Organic carbon stocks (kg/m²)

## Usage

``` r
get_soilgrids(layers, depths, stats)
```

## Source

<https://isric.org/explore/soilgrids>

## Arguments

- layers:

  A character vector indicating the layers to download from soilgrids

- depths:

  A character vector indicating the depths to download

- stats:

  A character vector indicating the statistics to download.

## Value

A function that returns an `sf` footprint object.

## Details

Except for `ocs`, which is only available for a depth of `"0-30cm"`, all
other parameters are available at the following depths:

- "0-5cm"

- "5-15cm"

- "15-30cm"

- "30-60cm"

- "60-100cm"

- "100-200cm"

Each parameter and depth is available for the following statistics:

- "Q0.05"

- "Q0.50"

- "mean"

- "Q0.95"

## References

Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen,
B., Ribeiro, E., and Rossiter, D.: SoilGrids 2.0: producing soil
information for the globe with quantified spatial uncertainty, SOIL, 7,
217–240, 2021.
[doi:10.5194/soil-7-217-2021](https://doi.org/10.5194/soil-7-217-2021)
