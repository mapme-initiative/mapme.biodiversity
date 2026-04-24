# Global Surface Water Change

The Global Surface Water dataset was developed by the European
Commission's Joint Research Centre in the framework of the Copernicus
Programme. It maps the location and temporal distribution of water
surfaces at the global scale over the past 3.8 decades and provides
statistics on their extent and change. It is provisioned as a global
tiled raster resource available for all land areas. The reported data
represent aggregated observations between 1984 - 2021.

## Usage

``` r
get_global_surface_water_change(version = "v1_4_2021")
```

## Source

<https://global-surface-water.appspot.com/>

## Arguments

- version:

  A character vector indicating the version of the GSW data set to make
  available.

## Value

A function that returns an `sf` footprint object.

## Details

The change in water occurrence intensity between the two periods is
derived from homologous pairs of months (i.e. same months containing
valid observations in both periods). The difference in the occurrence of
surface water was calculated for each homologous pair of months. The
average of all of these differences constitutes the Surface Water
Occurrence change intensity. The raster files have integer cell values
between `[0, 200]` where 0 represents surface water loss and 200
represents surface water gain.

## References

Pekel, JF., Cottam, A., Gorelick, N. et al. High-resolution mapping of
global surface water and its long-term changes. Nature 540, 418–422
(2016). [doi:10.1038/nature20584](https://doi.org/10.1038/nature20584)
