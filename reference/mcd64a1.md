# MODIS Burned Area Monthly (MCD64A1)

The Terra and Aqua combined MCD64A1 Version 6.1 Burned Area data product
is a monthly, global gridded 500 meter (m) product containing per-pixel
burned-area and quality information. The MCD64A1 burned-area mapping
approach employs 500 m Moderate Resolution Imaging Spectroradiometer
(MODIS) Surface Reflectance imagery coupled with 1 kilometre (km) MODIS
active fire observations.

## Usage

``` r
get_mcd64a1(years = 2000:2022)
```

## Source

<https://planetarycomputer.microsoft.com/dataset/modis-64A1-061>

## Arguments

- years:

  Numeric vector of years to make the MCD64A1 product available for.
  Must be greater than the year 2000.

## Value

A function that returns an `sf` footprint object.

## Details

The algorithm uses a burn sensitive Vegetation Index (VI) to create
dynamic thresholds that are applied to the composite data. The VI is
derived from MODIS shortwave infrared atmospherically corrected surface
reflectance bands 5 and 7 with a measure of temporal texture. The
algorithm identifies the date of burn for the 500 m grid cells within
each individual MODIS tile. The date is encoded in a single data layer
as the ordinal day of the calendar year on which the burn occurred with
values assigned to unburned land pixels and additional special values
reserved for missing data and water grid cells.

## References

Giglio, L., C. Justice, L. Boschetti, D. Roy. MODIS/Terra+Aqua Burned
Area Monthly L3 Global 500m SIN Grid V061. 2021, distributed by NASA
EOSDIS Land Processes Distributed Active Archive Center.
[doi:10.5067/MODIS/MCD64A1.061](https://doi.org/10.5067/MODIS/MCD64A1.061)
