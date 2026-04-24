# Global Surface Water Occurrence

The Global Surface Water dataset was developed by the European
Commission's Joint Research Centre in the framework of the Copernicus
Programme. It maps the location and temporal distribution of water
surfaces at the global scale over the past 3.8 decades and provides
statistics on their extent and change. It is provisioned as a global
tiled raster resource available for all land areas. The reported data
represent aggregated observations between 1984 - 2021.

## Usage

``` r
get_global_surface_water_occurrence(version = "v1_4_2021")
```

## Source

<https://global-surface-water.appspot.com/>

## Arguments

- version:

  A character vector indicating the version of the GSW data set to make
  available.

## Value

A character of file paths.

## Details

GSW occurrence raw data comes in raster files with integer cell values
between `[0, 100]`. This value gives the percentage of the time that a
given pixel was classified as water during the entire observation
period. So a 0 denotes a pixel that was never classified as water, 100
denotes a pixel with permanent water.

## References

Pekel, JF., Cottam, A., Gorelick, N. et al. High-resolution mapping of
global surface water and its long-term changes. Nature 540, 418–422
(2016). [doi:10.1038/nature20584](https://doi.org/10.1038/nature20584)
