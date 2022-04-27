# mapme.biodiversity (development version)

## Breaking changes

## New features

## Internal

# mapme.biodiversity 0.1.0

## Breaking changes
* renamed '.assetid' to 'assetid' (#22)

## New features
* None

## Internal
* ensured that tests and examples adhere to CRAN policies of
only writing to the temporal directory (#22)

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
* \value tags added to all exported functions explaining what is the output/sideffect (#59)
* using requireNamespace() instead of installed.packages() to check if packages listed in SUGGEST are loadable (#58)
