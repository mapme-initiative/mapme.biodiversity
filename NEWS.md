# mapme.biodiversity (development version)

# mapme.biodiversity 0.2.1

## Bug fixes
- fixes a serious bug that occurred for tiled resources when multiple assets are within 
the same tile resulting in returning the same tile multiple times

## Internal
- tests to catch above mentioned bug have been introduced for tiled resources

# mapme.biodiversity 0.2.0

## Breaking changes
* extensive renaming of resources and indicators. These are handled gracefully
until the next release (i.e. a warning is issued and names are replaced):

* resources:
* `treecover2000` -> `gfw_treecover`
* `lossyear` -> `gfw_lossyear`
* `greenhouse` -> `gfw_emissions`
* `traveltime` -> `nelson_et_al`
* `nasagrace` -> `nasa_grace` 
* `mintemperature` -> `worldclim_min_temperature`
* `maxtemperature` -> `worldclim_max_temperature`
* `precipitation` -> `worldclim_precipitation`
* `ecoregions` -> `teow`
* `mangrove` -> `gmw`
* `srtmdem` -> `nasa_srtm`

* indicators:
* `treecover` -> `treecover_area`
* `emissions` -> `treecoverloss_emissions`
* `treeloss` -> `treecover_area_and_emissions`
* `chirpsprec` -> `precipitation_chirps`
* `accessibility` -> `traveltime`
* `popcount` -> `population_count`
* `wctmin` -> `temperature_min_wc`
* `wctmax` -> `temperature_max_wc`
* `wcprec` -> `precipitation_wc`
* `gmw` -> `mangroves_area`
* `teow` -> `ecoregion`

## New features

- new resource(s):
  - nasa_firms
- new indicator(s):
  - active_fire_properties
  - active_fire_counts

## Internal

* adapted download routine to GMW v3 (#80)
* removed data.table from imports

## Bug fixes

* fixing issue #84 concerning intersection of tiled datasets (#86, @Jo-Schie)


# mapme.biodiversity 0.1.2

## Internal
* unit tests have been silenced in order to be more informative for reverse
dependency checks
* checks for tile availability reactivated for SRTM
* fixed notes due to uninitialized variables in TEOW and biome indicators

# mapme.biodiversity 0.1.1

## Internal
* `init_portfolio()` now sets the `testing` attribute to FALSE by default.
* `get_<resource>()` functions now return filenames early if `testing` is set to TRUE.
* `calc_<indicator>()` examples now copy files to the R temporal directory and are 
wrapped in `try()` to avoid errors/warnings on CRAN if an internet resource is not available.
* examples for `calc_tri()` and `calc_elevation()` are now disabled on CRAN because
of the responsiveness of CIGAR servers.

# mapme.biodiversity 0.1.0

## Breaking changes
* renamed '.assetid' to 'assetid' (#22)

## New features
* None

## Internal
* ensures that tests and examples adhere to CRAN policies of
only writing to the temporal directory (#22).

# mapme.biodiversity 0.0.1

## Initial release

* Added a `NEWS.md` file to track changes to the package.
* initial supported resources are:
  - ecoregions
  - esalandcover
  - greenhouse
  - lossyear
  - mangrove
  - nasagrace
  - soilgrids
  - srtmdem
  - traveltime
  - treecover
  - worldclim
  - worldpop
* initial supported indicators are: 
  - acessibility
  - biome
  - chirpsprec
  - drought_indicator
  - elevation
  - emissions
  - gmw
  - landcover
  - popcount
  - soilproperties
  - teow
  - treecover
  - treeloss
  - tri
  - wcprec
  - wctmax
  - wctmin
* `init_portfolio()` is used to initialize a portfolio object. The input must be
  an sf object where all geometries are of type `POLYGON`
* users can request the download of one or more resources via `get_resources()`
* users can request the processing of an indicator via `calc_indicators()`
* indicators are added to the portfolio object as nested list columns
* a processed portfolio object can be exported as a GeoPackage via `write_portfolio()`
* a portfolio saved to disk as a GeoPackage can be read back into R via `read_portfolio()`.
  If users wish to download additional resources or calculate indicators, `init_portfolio()`
  has to be called again.
* Parallelization using multiple cores on the host machine is disabled on Windows
* on MacOS, the s2 engine for spherical geometric vector operations is disabled and
  lwgeom is used instead.
  
## Internal

* Introduced absolute URLS in userguide.Rmd pointing to the online documentation (#59)
* \value tags added to all exported functions explaining what is the output/side effect (#59)
* using requireNamespace() instead of installed.packages() to check if packages listed in SUGGEST are loadable (#58)
