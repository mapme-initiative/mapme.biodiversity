# Forest greenhouse gas emissions

This resource is part of the publication by Harris et al. (2021) "Global
maps of twenty-first century forest carbon fluxes.". It represents "the
greenhouse gas emissions arising from stand-replacing forest
disturbances that occurred in each modelled year (megagrams CO2
emissions/ha, between 2001 and 2023). Emissions include all relevant
ecosystem carbon pools (aboveground biomass, belowground biomass, dead
wood, litter, soil) and greenhouse gases (CO2, CH4, N2O)." The area unit
that is downloaded here corresponds to the "megagrams of CO2
emissions/pixel" layer, in order to support the calculation of area-wise
emissions.

## Usage

``` r
get_gfw_emissions()
```

## Source

<https://data.globalforestwatch.org/datasets/gfw::forest-greenhouse-gas-emissions/about>

## Value

A function that returns an `sf` footprint object.

## Details

There are no arguments users need to specify. However, users should note
that the spatial extent for this dataset does not totally cover the same
extent as the `treecover2000` and `lossyear` resources by Hansen et al.
(2013). A missing value (NA) will be inserted for greenhouse gas
emissions for areas where no data is available.

## References

Harris, N.L., Gibbs, D.A., Baccini, A. et al. Global maps of
twenty-first century forest carbon fluxes. Nat. Clim. Chang. 11, 234–240
(2021).
[doi:10.1038/s41558-020-00976-6](https://doi.org/10.1038/s41558-020-00976-6)
