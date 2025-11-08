# Global Surface Water Transitions

The Global Surface Water dataset was developed by the European
Commission's Joint Research Centre in the framework of the Copernicus
Programme. It maps the location and temporal distribution of water
surfaces at the global scale over the past 3.8 decades and provides
statistics on their extent and change. It is provisioned as a global
tiled raster resource available for all land areas. The reported data
represent aggregated observations between 1984 - 2021.

## Usage

``` r
get_global_surface_water_transitions(version = "v1_4_2021")
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

GSW transition data contains information about the type of surface water
change for each pixel. The raster files have integer cell values between
`[0, 10]` that code for different transition classes:

|       |                       |
|-------|-----------------------|
| Value | Transition Class      |
| 1     | Permanent             |
| 2     | New Permanent         |
| 3     | Lost Permanent        |
| 4     | Seasonal              |
| 5     | New Seasonal          |
| 6     | Lost Seasonal         |
| 7     | Seasonal to Permanent |
| 8     | Permanent to Seasonal |
| 9     | Ephemeral Permanent   |
| 10    | Ephemeral Seasonal    |

## References

Pekel, JF., Cottam, A., Gorelick, N. et al. High-resolution mapping of
global surface water and its long-term changes. Nature 540, 418–422
(2016). [doi:10.1038/nature20584](https://doi.org/10.1038/nature20584)
