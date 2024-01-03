# mapme.biodiversity 0.5.0

## General

- Quickstart vignette uses the WorldPop resource instead of CHIRPS, not
  relying on a working internet connection (#230).
  
## New features

- GFW resources and indicators include latest GFC-2022-v1.10 version (#203).
- Raster resources with a CRS different from WGS84 are now supported (#213).

## Breaking changes

- The argument `add_resources` to `init_portfolio()` is deprecated. This
  means that `get_resources()` has to be run in every new R session to make 
  resource available for further processing (#219).
- Rasters are now cropped to the spatial extent of an asset with setting 
  `snap="out"`, thus delivering a slightly bigger extent (#212).
  
## Bug Fixes

- `calc_indicators()` checks for 0-length tibbles (#196, #199, #215).
- Fix bug with reading rasters with temporal dimensions (#209).
- All raster cells touching a polygon are now returned (#208).

## Internal

- `.read_raster_source()` now uses a simplified logic to cover all cases 
  (e.g. single tiles, tiled rasters with and without temporal dimension, 
  single temporal rasters) (#211).
- Rasters are cropped using `snap="out"` by default (#212).
- `.read_raster_source()` now projects assets in case their CRS differs 
  from the portfolio (#213).
- tile indices for raster resources are now appended to the portfolio attributes
  as `sf` objects instead of being written to disk (#219).
- `.read_raster_source()` now applies a precision round-trip of 5 decimal point 
  to match rasters with slight changes in their spatial extent (#217).
- `register_resource()` and `register_indicator()` now issue warnings for 
  resources/indicators with names already registered and overwrites them (#220).


# mapme.biodiversity 0.4.0

## New features

- added new resource called `ucdp_ged` providing a database of violent conflict
  from 1989 to today

- added a new indicator called `fatalities` aggregating number of deaths
  by type of conflict on a monthly time scale based on the `ucdp_ged` resource.
  
- Added a new resource called `fritz_et_al` providing a raster layer of deforestation

- added a new resource called `fritz_et_al` providing a raster layer of deforestation
  drivers in tropical forests based on [Fritz et al. (2022)](https://www.frontiersin.org/articles/10.3389/fcosc.2022.830248/full)

- added a new indicator called `deforestation_drivers` using the `fritz_et_al` resource
  to obtain information on the absolute and relative area driving forest losses in 
  assets for the period 2008-2019
  
- added two new exported functions `register_resource()` and `register_indicator()`
  which allow users to register custom functions for resources/indicators
  
- added a new vignette for the web-version of the package only informing
  about how to obtain wide-output from indicators
  
- added a new vignette for the web-version for a custom analysis of the NASA
  FIRMS resource in the example section

- added the data for years 2017-2020 to the Global Mangrove Watch resource 


## Breaking changes

- Changed the parallel backend to the [future](https://cran.r-project.org/package=future) 
  package. Parallel processing is now implemented by
  [furrr::future_map()](https://furrr.futureverse.org/reference/future_map.html)
  on the asset level within the calc_indicators() function. User code is now
  required to set up a [plan()](https://future.futureverse.org/reference/plan.html)
  to enable parallel processing. The function call needs to be wrapped on the user
  side with [progressr::with_progress()](https://progressr.futureverse.org/reference/with_progress.html)
  to show a progress bar.
  
- mapme.biodiversity no longer sets terra's temporal directory for you.
  Instead you have to call `terra::terraOptions()` manually

## Bug Fixes

- `esalandcover` indicator now returns the value per each land cover class 
  exactly once (#177)

## Internal

- disabled running examples on CRAN 

- disabled tests for get_* functions on CRAN

- `terra` engines now use `get()` to resolve the requested zonal
  statistic function

- applying tidyverse coding style to existing code (#156, @karpfen)

- extensive re-factoring of vector-raster zonal statistic engines (#150)

- extensive re-writing of testing infrastructure for indicator functions omitting
  the usage of snapshot tests as far as possible (#142)

- `rundir` and `todisk` arguments removed from indicator functions since they were
  of no practical use

- instead of a resource and indicator backlog, resources and indicators are now
  registered to .pkgenv and queried there during runtime. This also allows
  users to register custom resources/indicator functions
  
- removed deprecation warnings for old resource/indicator name
  


# mapme.biodiversity 0.3.0

## Breaking changes

- on MacOS s2-based calculations are now enabled so users can expect the package
  to return numerically equivalent results on any operating system (#131)
  
- the online source for the `nasa_srtm` resource shows an expired SSL certificate
  since November 2022. The `get_resources()` function now includes an error and 
  instructions how to disable SSL certification at a users own risk. The 
  websites maintainers have been contacted and asked to renew the certification. (#131)

## New features

- GFW resources are now updated to use the latest version allowing analysis for 
  the additional year of 2021 (#123, @fBedecarrats)
  
- GFW indicators now accept numeric `min_size` argument allowing to specify
  fractional covers (#110)
  
- fire indicators now allow the simultaneous calculation of indicators based on 
  MODIS and VIIRS. Before users had to chose between one of the instruments for
  each analysis (#126)
  
## Bug fixes

- case when one or multiple assets return NA instead of a tibble is now properly
  tested and handled (#101)

- Rasters are no longer temporary written to disk to omit a bug caused by 
  applying mask/classify to an already existing raster file (#108, @Jo-Schie)
  
- Bug with soilproperties set to NA caused by the function to return a data.frame
  instead of a tibble was fixed (#116)

- both, `treecoverloss_emissions` and `treecover_area_and_emissions` now return
  0 instead of NaN for observation years where now forest loss occurred (#120)
  
## Internal

- `.make_global_grid()` now specifies the CRS when constructing the bounding box
  and returns the grid in the specified CRS instead of Lat/Lon (#113)
 
- `.calc_active_fire_properties` now uses st_coordinates to retrieve locations
  of fires (#119, @DavisVaughan)
  
- tests for MacOS have been re-enabled (#131)

- tests for downloading `nasa_srtm` resource are skipped because the SSL certificate
  of the online source has expired (#131)


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
