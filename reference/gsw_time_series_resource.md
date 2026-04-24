# Helper function to download Global Surface Water (GSW) yearly time series data

This function constructs the necessary data URLs for a given data set,
version and polygon and downloads them for further processing with the
mapme.biodiversity package.

## Usage

``` r
get_gsw_time_series(years, version = "LATEST")
```

## Source

Raw Data:
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GSWE/YearlyClassification/LATEST/tiles/>

## Arguments

- years:

  Numeric vector of years to process between 1984 and 2021. Default:
  `1984:2021`.

- version:

  Version of the data set to process. Available options are (`VER1-0`,
  `VER2-0`, `VER3-0`, `VER4-0`, `VER5-0` and `LATEST`) Default:
  `LATEST`. Choosing `LATEST` will result in the latest available
  version.

## Value

A function that returns a character vector of file paths.

## Details

The available surface water classes for a given pixel are the following:

- No Observation: It was not possible to determine whether a pixel was
  water (this may be the case for frozen areas or during the polar night
  in extreme latitudes).

- Permanent Water: Water was detected in twelve months per year or in a
  combination of permanent and no observation.

- Seasonal Water: Water and no water was detected.

- No Water: No Water was detected.

## References

- Global Surface Water Explorer:
  <https://global-surface-water.appspot.com/>

- Data Users Guide:
  <https://storage.cloud.google.com/global-surface-water/downloads_ancillary/DataUsersGuidev2021.pdf>

- Research Article: <https://www.nature.com/articles/nature20584>
