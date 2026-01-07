# Changelog

## mapme.biodiversity 0.9.5.9001

### General

- modified
  [`get_nelson_et_al()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/nelson_et_al.md)
  to cope with Figshare asynchronous download mechanism
  ([474](https://github.com/mapme-initiative/mapme.biodiversity/issues/474))
- updated
  [`get_humanfootprint()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/humanfootprint_resource.md)
  to accept years up to 2024

## mapme.biodiversity 0.9.5.9000

### General

- modified
  [`get_acled()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/acled.md)
  to align with the new ACLED API version
  ([440](https://github.com/mapme-initiative/mapme.biodiversity/issues/440))
- updated tests related to ACLED resource to reflect the changes in the
  [`get_acled()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/acled.md)
  function

## mapme.biodiversity 0.9.5

CRAN release: 2025-10-06

### General

- [`get_gfw_treecover()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gfw_treecover.md)
  and
  [`get_gfw_lossyear()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gfw_lossyear.md)
  updated to version “GFC-2024-v1.12”
  ([416](https://github.com/mapme-initiative/mapme.biodiversity/issues/416))
- [`calc_treecover_area()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/treecover_area.md),
  [`calc_treecover_area_and_emissions()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/treecover_area_and_emissions.md)
  and
  [`calc_treecoverloss_emissions()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/treecoverloss_emissions.md)
  updated to include the year 2024
  ([416](https://github.com/mapme-initiative/mapme.biodiversity/issues/416))
- [`get_esalandcover()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/esalandcover.md)
  has been adapted to download data from a new source in a format
  compatible with the previous version
  ([430](https://github.com/mapme-initiative/mapme.biodiversity/issues/430))
- modified
  [`calc_treecover_area()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/treecover_area.md)
  to return zero area and not NULL when tree cover \< `min_cover`
  ([413](https://github.com/mapme-initiative/mapme.biodiversity/issues/413))
- [`get_teow()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/teow.md)
  now requires users to manually download the file
  ([450](https://github.com/mapme-initiative/mapme.biodiversity/issues/450))

### Internal

- internal function `.has_internet()` now delegates to
  [`curl::has_internet()`](https://jeroen.r-universe.dev/curl/reference/nslookup.html)
- added function `.check_system_requirements()` that checks on package
  load if system requirements (GDAL and PROJ versions) are met, and
  warns if not
- use
  [`utils::compareVersion()`](https://rdrr.io/r/utils/compareVersion.html)
  instead of simple ‘\<’ to check the GDAL system library version

### Bug fixes

- internal function `.fetch_resource()` now correctly takes into account
  the resources that could not be retrieved and reports them
  ([433](https://github.com/mapme-initiative/mapme.biodiversity/issues/433))
- fixed messages from the
  [`check_namespace()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/check_namespace.md)
  function
- fixed the way
  [`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) is used
  so it works correctly on Windows
  ([446](https://github.com/mapme-initiative/mapme.biodiversity/issues/446))
- fixed
  [`calc_treecover_area()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/treecover_area.md)
  to correctly handle the full tree cover (100%)
  ([451](https://github.com/mapme-initiative/mapme.biodiversity/issues/451))

## mapme.biodiversity 0.9.4

CRAN release: 2025-04-28

### General

- [`get_biodiversity_intactness_index()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/biodiversity_intactness_index_resource.md),
  [`get_iucn()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/iucn.md),
  and
  [`get_key_biodiversity_areas()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/key_biodiversity_areas_resource.md)
  now use
  [`spds_exists()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/spds_exists.md)
  to check if input files exist
- [`get_chelsa()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/chelsa.md)
  now supports precipitation layers for 2019
- changes bucket URL for
  [`get_chelsa()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/chelsa.md)
  to `https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/pr`
- `spds_exist()` and
  [`make_footprints()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/make_footprints.md)
  now use [`normalizePath()`](https://rdrr.io/r/base/normalizePath.html)
  allowing better handling of relative file paths

### Internal

- the `sf_col` attribute value is now retained throughout the portfolio
  checks and chunking routines

- `.raster_bbox()` now only uses the `cornerCoordinates` output from
  `gdalinfo` to derive a bounding box
  ([385](https://github.com/mapme-initiative/mapme.biodiversity/issues/385))

- bounding boxes for raster and vector resources are now derived via
  `st_as_sfc(st_bbox(x))` to ensure that they are oriented correctly
  when using S2
  ([377](https://github.com/mapme-initiative/mapme.biodiversity/issues/377)
  and
  [385](https://github.com/mapme-initiative/mapme.biodiversity/issues/385))

## mapme.biodiversity 0.9.3

CRAN release: 2024-10-21

### Bug fixes

- [`get_nasa_srtm()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/nasa_srtm.md)
  now uses GDAL’s VSI path option `pc_url_signing=yes` to sign URLs from
  Microsoft Planetary Computer
  ([383](https://github.com/mapme-initiative/mapme.biodiversity/issues/383))

### Internal

- test for `.read_vector()` now copies input GPKG to a directory with
  write permissions to avoid CRAN check failures when included in a read
  only directory

## mapme.biodiversity 0.9.2

CRAN release: 2024-10-10

### General

- new resources:
  - [`get_acled()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/acled.md)
- new indicators:
  - [`calc_fatalities_acled()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/fatalities_acled.md)
  - [`calc_fatalities_ucdp()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/fatalities_ucpd.md)
    (renamed)
  - [`calc_exposed_population_acled()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/calc_exposed_population_acled.md)
  - [`calc_exposed_population_ucdp()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/exposed_population_ucdp.md)
    (renamed)
- [`calc_fatalities_ucdp()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/fatalities_ucpd.md)
  now returns a sparse timeseries, e.g. asset-months with now fatalities
  are omitted.

### Bug fixes

- fixes
  [`portfolio_wide()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
  throwing an error when single assets with `NULL` values are present
- [`calc_mangroves_area()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mangroves_area.md)
  returned NULL if invalid geometries were encountered Now it tries to
  repair geometries and return the area of valid geometries
  ([375](https://github.com/mapme-initiative/mapme.biodiversity/issues/375))

### Internal

- `.get_intersection()` now assumes both `x` and `tindex` to be
  represented by oriented rings on the sphere
  ([378](https://github.com/mapme-initiative/mapme.biodiversity/issues/378))

## mapme.biodiversity 0.9.1

CRAN release: 2024-09-02

### General

- new resources:
  - [`get_accessibility_2000()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/accessibility_2000.md)
    ([365](https://github.com/mapme-initiative/mapme.biodiversity/issues/365),
    [@fBedecarrats](https://github.com/fBedecarrats))
- new indicators:
  - [`calc_traveltime_2000()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/traveltime_2000.md)
    ([365](https://github.com/mapme-initiative/mapme.biodiversity/issues/365),
    [@fBedecarrats](https://github.com/fBedecarrats))

### Internal

- adjusts test for `get_gsw_timseries()` and `calc_gsw_timeseries()` to
  write to temporal directory of the R session to fix CRAN errors
  ([370](https://github.com/mapme-initiative/mapme.biodiversity/issues/370),
  [@karpfen](https://github.com/karpfen))

## mapme.biodiversity 0.9.0

CRAN release: 2024-08-27

### General

- [`prep_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  received additional argument `mode` to get control over the reading
  mode (e.g. portfolio or asset)

- resources based on WorldClim now support selecting the spatial
  resolution and cover the historical timeseries starting from 1960
  ([302](https://github.com/mapme-initiative/mapme.biodiversity/issues/302))

- assets are now chunked into sub-components prior to indicator
  calculation thus parallelization now is applied to a single level
  ([322](https://github.com/mapme-initiative/mapme.biodiversity/issues/322))

- `chunk_size` now is properly set to 100,000 ha as per documentation
  (before it was set to 10,000 ha)
  ([324](https://github.com/mapme-initiative/mapme.biodiversity/issues/324))

- setting `chunk_size=NULL` is now allowed and skips chunking
  ([331](https://github.com/mapme-initiative/mapme.biodiversity/issues/331))

- treecover indicators now trough a message if landscapemetrics is not
  installed
  ([325](https://github.com/mapme-initiative/mapme.biodiversity/issues/325))

- setting `outdir` via
  [`mapme_options()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  now probes the destination by trying to write a GTiff file and errors
  if unsuccessful
  ([335](https://github.com/mapme-initiative/mapme.biodiversity/issues/335))

- code previously using `httr` now uses `httr2`
  ([330](https://github.com/mapme-initiative/mapme.biodiversity/issues/330))

- new resources:

  - [`get_iucn()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/iucn.md)
    ([359](https://github.com/mapme-initiative/mapme.biodiversity/issues/359))
  - [`get_chelsa()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/chelsa.md)
    ([318](https://github.com/mapme-initiative/mapme.biodiversity/issues/318))
  - [`get_ipbes_biomes()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/ipbes_biomes.md)
    ([345](https://github.com/mapme-initiative/mapme.biodiversity/issues/345))
  - [`get_humanfootprint()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/humanfootprint_resource.md)
    ([341](https://github.com/mapme-initiative/mapme.biodiversity/issues/341))
  - [`get_gsw_time_series()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gsw_time_series_resource.md)
    ([354](https://github.com/mapme-initiative/mapme.biodiversity/issues/354),
    [@karpfen](https://github.com/karpfen))
  - [`get_key_biodiversity_areas()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/key_biodiversity_areas_resource.md)
    ([349](https://github.com/mapme-initiative/mapme.biodiversity/issues/349),
    [@karpfen](https://github.com/karpfen))
  - [`get_biodiversity_intactness_index()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/biodiversity_intactness_index_resource.md)
    ([351](https://github.com/mapme-initiative/mapme.biodiversity/issues/351),
    [@karpfen](https://github.com/karpfen))
  - [`get_vul_carbon()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/carbon_resources.md),
    [`get_man_carbon()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/carbon_resources.md),
    and
    [`get_irr_carbon()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/carbon_resources.md)
    ([339](https://github.com/mapme-initiative/mapme.biodiversity/issues/339))

- new indicators:

  - [`calc_slope()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/slope.md)
    ([355](https://github.com/mapme-initiative/mapme.biodiversity/issues/355),
    [@fBedecarrats](https://github.com/fBedecarrats))
  - [`calc_ipbes_biomes()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/ipbes_biome_stats.md)
    ([345](https://github.com/mapme-initiative/mapme.biodiversity/issues/345))
  - [`calc_humanfootprint()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/humanfootprint_indicator.md)
    ([341](https://github.com/mapme-initiative/mapme.biodiversity/issues/341))
  - [`calc_gsw_time_series()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gsw_time_series_indicator.md)
    ([354](https://github.com/mapme-initiative/mapme.biodiversity/issues/354),
    [@karpfen](https://github.com/karpfen))
  - [`calc_species_richness()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/species_richness.md)
    ([359](https://github.com/mapme-initiative/mapme.biodiversity/issues/359))
  - `calc_exposed_population()`
    ([321](https://github.com/mapme-initiative/mapme.biodiversity/issues/321))
  - [`calc_precipitation_chelsa()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/precipitation_chelsa.md)
    ([318](https://github.com/mapme-initiative/mapme.biodiversity/issues/318))
  - [`calc_key_biodiversity_area()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/key_biodiversity_areas_indicator.md)
    ([349](https://github.com/mapme-initiative/mapme.biodiversity/issues/349),
    [@karpfen](https://github.com/karpfen))
  - [`calc_biodiversity_intactness_index()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/biodiversity_intactness_index_indicator.md)
    ([351](https://github.com/mapme-initiative/mapme.biodiversity/issues/351),
    [@karpfen](https://github.com/karpfen))
  - [`calc_vul_carbon()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/carbon_indicators.md),
    [`calc_man_carbon()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/carbon_indicators.md),
    and
    [`calc_irr_carbon()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/carbon_indicators.md)
    ([339](https://github.com/mapme-initiative/mapme.biodiversity/issues/339))

### Bug fixes

- fixes transforming asset to the CRS of raster dataset in
  [`calc_deforestation_drivers()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/deforestation_drivers.md)
  ([300](https://github.com/mapme-initiative/mapme.biodiversity/issues/300))
- [`write_portfolio()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
  now drops indicators with only NULL values instead of throwing an
  error
  ([303](https://github.com/mapme-initiative/mapme.biodiversity/issues/303))
- [`get_ucdp_ged()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/ucdp_ged.md)
  now adds SRS infos to the footprints object
  ([313](https://github.com/mapme-initiative/mapme.biodiversity/issues/313))
- uses binary writing mode for `worldpop` resource on Windows
  ([319](https://github.com/mapme-initiative/mapme.biodiversity/issues/319))

### Internal

- `.check_portfolio()` now checks if `assetid` has unique values and
  only overrides them if this in not the case
  ([305](https://github.com/mapme-initiative/mapme.biodiversity/issues/305))
- `.read_raster()` now reads values into memory and removes VRT files
  on-exit
  ([311](https://github.com/mapme-initiative/mapme.biodiversity/issues/311))
- `.fetch_resources()` now honours both creation and opening options
  ([315](https://github.com/mapme-initiative/mapme.biodiversity/issues/315))
- `httr` calls are replaced with the respective `httr2` equivalents
  ([329](https://github.com/mapme-initiative/mapme.biodiversity/issues/329))

## mapme.biodiversity 0.8.0

CRAN release: 2024-07-03

### General

- updates `gfw_lossyear` resource to `v20240402` which entails emission
  data between 2000 - 2023
- removes the `nasa_firms` resource and associated `active_fire_counts`
  indicator
- adds `mcd64a1` resource and `burned_area` indicator
- `mapme.biodiveristy` now leverages GDAL for data I/O meaning that all
  GDAL readable source data sets and writable destinations are now
  supported
- `README.md` now includes a section on how to set up cloud-storages to
  use as a destination for resource data
- The quickstart vignette now uses GFW data as example data
- chunking is now applied based on the area of an assets bounding box
  instead of its own area
- [`write_portfolio()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
  now again serializes to a two-table GeoPackage and re-introduces
  [`read_portfolio()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
  ([294](https://github.com/mapme-initiative/mapme.biodiversity/issues/294))
- `datetime` column values are now encoded as `POSIXct`

### Internal

- exports
  [`make_footprints()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/make_footprints.md)
  to ease the process of creating footprints for resource functions
- exports
  [`spds_exists()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/spds_exists.md)
  for resource function to check if a data source is exists
- `get_*()` functions are now required to return footprint objects
  indicating the spatial extent of each elements and pointing towards a
  GDAL readable data source in the `source` column
- in case a user-specified destination is found, the package now uses
  `gdal_translate` to write data from source to destination
- tests for long-running examples and tests are skipped on GA and CRAN
- fixes a bug in checking if a portfolio inherits from `tbl_df`

## mapme.biodiversity 0.7.0

CRAN release: 2024-05-31

### Bug fixes

- fixes bug with wrong tile paths returned by
  [`get_gfw_emissions()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/gfw_emissions.md)

### Breaking changes

- introduces a standardized output format for indicators, see
  [240](https://github.com/mapme-initiative/mapme.biodiversity/issues/240)
  for more information
- [`get_chirps()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/chirps.md)
  now allows to specify the years for which to download CHIRPS resources
- [`calc_precipitation_chirps()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/precipitation_chirps.md)
  now only returns precipitation sums
- deprecation of indicator `active_fire_properties` since resources can
  now be retrieved using
  [`prep_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  (see below)

### General

- exports
  [`prep_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  to prepare resources for single assets
- exports
  [`portfolio_long()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
  and
  [`portfolio_wide()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
  to automatically unnest indicator columns and change the data layout
  to either long or wide
- changes the behaviour of
  [`write_portfolio()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
  to serialize portfolios to GDAL supported spatial formats in either
  long or wide format and deprecates
  [`read_portfolio()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
- introduces option `chunk_size` to
  [`mapme_options()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  to control the size for which polygons are split and processed in
  chunks
- allows assets of type `'MULTIPOLYGON'` and automatically combines the
  results based on an aggregation function
- indicator examples now use
  [`portfolio_long()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
  instead of
  [`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)

### Internal

- indicator functions must now return tibbles with columns named
  `datetime`, `variable`, `unit` and `value`
- inner-level indicator functions must now specify a statistic for
  aggregation of chunks
- `chirps` and `nasa_grace` resources updated
- check for internet connectivity can now be disabled via environment
  variable `mapme_check_connection`
  ([262](https://github.com/mapme-initiative/mapme.biodiversity/issues/262))
- `gfw_treecover` and `gfw_lossyear` resources updated to v1.11
  ([277](https://github.com/mapme-initiative/mapme.biodiversity/issues/277),
  [@fBedecarrats](https://github.com/fBedecarrats))
- GFW indicators now automatically detect the maximum years based on the
  `gfw_lossyear` layer
  ([273](https://github.com/mapme-initiative/mapme.biodiversity/issues/273))
- drops `curl`, `stringr`, and `tidyselect` as dependencies
- moves `progressr` and `rvest` from `Imports` to `Suggests`
- drops `SPEI` from `Suggests`

## mapme.biodiversity 0.6.0

CRAN release: 2024-04-30

### Breaking changes

- introduces a new UI based on closures for resources and indicators,
  see
  [240](https://github.com/mapme-initiative/mapme.biodiversity/issues/240)
  for more information

### General

- improves output of
  [`available_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/resources.md)
  and
  [`available_indicators()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/indicators.md)
- introduces
  [`mapme_options()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  to add fine-control of the packages behaviour
- deprecates `init_portfolio()` in favour of
  [`mapme_options()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
- exports helper functions for third parties to extend the package for
  custom resources and indicators:
  - [`check_available_years()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/check_available_years.md)
  - [`check_namespace()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/check_namespace.md)
  - `download_or_skip()`
  - [`check_engine()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/engine.md)
  - [`check_stats()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/engine.md)
  - [`select_engine()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/engine.md)
  - [`make_global_grid()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/make_global_grid.md)
  - `unzip_and_remove()`

### New features

- added Global Surface Water resources and respective indicators
  ([235](https://github.com/mapme-initiative/mapme.biodiversity/issues/235),
  [@karpfen](https://github.com/karpfen))

### Internal

- removed
  [`st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
  from `.read_vector()`.

## mapme.biodiversity 0.5.0

CRAN release: 2024-01-08

### General

- Quickstart vignette uses the WorldPop resource instead of CHIRPS, not
  relying on a working internet connection
  ([230](https://github.com/mapme-initiative/mapme.biodiversity/issues/230)).

### New features

- GFW resources and indicators include latest GFC-2022-v1.10 version
  ([203](https://github.com/mapme-initiative/mapme.biodiversity/issues/203)).
- Raster resources with a CRS different from WGS84 are now supported
  ([213](https://github.com/mapme-initiative/mapme.biodiversity/issues/213)).

### Breaking changes

- The argument `add_resources` to `init_portfolio()` is deprecated. This
  means that
  [`get_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  has to be run in every new R session to make resource available for
  further processing
  ([219](https://github.com/mapme-initiative/mapme.biodiversity/issues/219)).

- Rasters are now cropped to the spatial extent of an asset with setting
  `snap="out"`, thus delivering a slightly bigger extent
  ([212](https://github.com/mapme-initiative/mapme.biodiversity/issues/212)).

- Speed improvements for GFW indicators (up to x10 for larger rasters)
  now require R package `exactextractr` to be installed. Also, it is
  advised to have the R package `landscapemetrics` installed to gain the
  full computation speed improvement.

### Bug Fixes

- [`calc_indicators()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  checks for 0-length tibbles
  ([196](https://github.com/mapme-initiative/mapme.biodiversity/issues/196),
  [199](https://github.com/mapme-initiative/mapme.biodiversity/issues/199),
  [215](https://github.com/mapme-initiative/mapme.biodiversity/issues/215)).
- Fix bug with reading rasters with temporal dimensions
  ([209](https://github.com/mapme-initiative/mapme.biodiversity/issues/209)).
- All raster cells touching a polygon are now returned
  ([208](https://github.com/mapme-initiative/mapme.biodiversity/issues/208)).

### Internal

- `.read_raster_source()` now uses a simplified logic to cover all cases
  (e.g. single tiles, tiled rasters with and without temporal dimension,
  single temporal rasters)
  ([211](https://github.com/mapme-initiative/mapme.biodiversity/issues/211)).
- Rasters are cropped using `snap="out"` by default
  ([212](https://github.com/mapme-initiative/mapme.biodiversity/issues/212)).
- `.read_raster_source()` now projects assets in case their CRS differs
  from the portfolio
  ([213](https://github.com/mapme-initiative/mapme.biodiversity/issues/213)).
- tile indices for raster resources are now appended to the portfolio
  attributes as `sf` objects instead of being written to disk
  ([219](https://github.com/mapme-initiative/mapme.biodiversity/issues/219)).
- `.read_raster_source()` now applies a precision round-trip of 5
  decimal point to match rasters with slight changes in their spatial
  extent
  ([217](https://github.com/mapme-initiative/mapme.biodiversity/issues/217)).
- [`register_resource()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/resources.md)
  and
  [`register_indicator()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/indicators.md)
  now issue warnings for resources/indicators with names already
  registered and overwrites them
  ([220](https://github.com/mapme-initiative/mapme.biodiversity/issues/220)).

## mapme.biodiversity 0.4.0

CRAN release: 2023-08-28

### New features

- added new resource called `ucdp_ged` providing a database of violent
  conflict from 1989 to today

- added a new indicator called `fatalities` aggregating number of deaths
  by type of conflict on a monthly time scale based on the `ucdp_ged`
  resource.

- Added a new resource called `fritz_et_al` providing a raster layer of
  deforestation

- added a new resource called `fritz_et_al` providing a raster layer of
  deforestation drivers in tropical forests based on [Fritz et
  al. (2022)](https://www.frontiersin.org/articles/10.3389/fcosc.2022.830248/full)

- added a new indicator called `deforestation_drivers` using the
  `fritz_et_al` resource to obtain information on the absolute and
  relative area driving forest losses in assets for the period 2008-2019

- added two new exported functions
  [`register_resource()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/resources.md)
  and
  [`register_indicator()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/indicators.md)
  which allow users to register custom functions for
  resources/indicators

- added a new vignette for the web-version of the package only informing
  about how to obtain wide-output from indicators

- added a new vignette for the web-version for a custom analysis of the
  NASA FIRMS resource in the example section

- added the data for years 2017-2020 to the Global Mangrove Watch
  resource

### Breaking changes

- Changed the parallel backend to the
  [future](https://cran.r-project.org/package=future) package. Parallel
  processing is now implemented by
  [furrr::future_map()](https://furrr.futureverse.org/reference/future_map.html)
  on the asset level within the calc_indicators() function. User code is
  now required to set up a
  [plan()](https://future.futureverse.org/reference/plan.html) to enable
  parallel processing. The function call needs to be wrapped on the user
  side with
  [progressr::with_progress()](https://progressr.futureverse.org/reference/with_progress.html)
  to show a progress bar.

- mapme.biodiversity no longer sets terra’s temporal directory for you.
  Instead you have to call
  [`terra::terraOptions()`](https://rspatial.github.io/terra/reference/terraOptions.html)
  manually

### Bug Fixes

- `esalandcover` indicator now returns the value per each land cover
  class exactly once
  ([177](https://github.com/mapme-initiative/mapme.biodiversity/issues/177))

### Internal

- disabled running examples on CRAN

- disabled tests for get\_\* functions on CRAN

- `terra` engines now use [`get()`](https://rdrr.io/r/base/get.html) to
  resolve the requested zonal statistic function

- applying tidyverse coding style to existing code
  ([156](https://github.com/mapme-initiative/mapme.biodiversity/issues/156),
  [@karpfen](https://github.com/karpfen))

- extensive re-factoring of vector-raster zonal statistic engines
  ([150](https://github.com/mapme-initiative/mapme.biodiversity/issues/150))

- extensive re-writing of testing infrastructure for indicator functions
  omitting the usage of snapshot tests as far as possible
  ([142](https://github.com/mapme-initiative/mapme.biodiversity/issues/142))

- `rundir` and `todisk` arguments removed from indicator functions since
  they were of no practical use

- instead of a resource and indicator backlog, resources and indicators
  are now registered to .pkgenv and queried there during runtime. This
  also allows users to register custom resources/indicator functions

- removed deprecation warnings for old resource/indicator name

## mapme.biodiversity 0.3.0

CRAN release: 2023-01-21

### Breaking changes

- on MacOS s2-based calculations are now enabled so users can expect the
  package to return numerically equivalent results on any operating
  system
  ([131](https://github.com/mapme-initiative/mapme.biodiversity/issues/131))

- the online source for the `nasa_srtm` resource shows an expired SSL
  certificate since November 2022. The
  [`get_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
  function now includes an error and instructions how to disable SSL
  certification at a users own risk. The websites maintainers have been
  contacted and asked to renew the certification.
  ([131](https://github.com/mapme-initiative/mapme.biodiversity/issues/131))

### New features

- GFW resources are now updated to use the latest version allowing
  analysis for the additional year of 2021
  ([123](https://github.com/mapme-initiative/mapme.biodiversity/issues/123),
  [@fBedecarrats](https://github.com/fBedecarrats))

- GFW indicators now accept numeric `min_size` argument allowing to
  specify fractional covers
  ([110](https://github.com/mapme-initiative/mapme.biodiversity/issues/110))

- fire indicators now allow the simultaneous calculation of indicators
  based on MODIS and VIIRS. Before users had to chose between one of the
  instruments for each analysis
  ([126](https://github.com/mapme-initiative/mapme.biodiversity/issues/126))

### Bug fixes

- case when one or multiple assets return NA instead of a tibble is now
  properly tested and handled
  ([101](https://github.com/mapme-initiative/mapme.biodiversity/issues/101))

- Rasters are no longer temporary written to disk to omit a bug caused
  by applying mask/classify to an already existing raster file
  ([108](https://github.com/mapme-initiative/mapme.biodiversity/issues/108),
  [@Jo-Schie](https://github.com/Jo-Schie))

- Bug with soilproperties set to NA caused by the function to return a
  data.frame instead of a tibble was fixed
  ([116](https://github.com/mapme-initiative/mapme.biodiversity/issues/116))

- both, `treecoverloss_emissions` and `treecover_area_and_emissions` now
  return 0 instead of NaN for observation years where now forest loss
  occurred
  ([120](https://github.com/mapme-initiative/mapme.biodiversity/issues/120))

### Internal

- `.make_global_grid()` now specifies the CRS when constructing the
  bounding box and returns the grid in the specified CRS instead of
  Lat/Lon
  ([113](https://github.com/mapme-initiative/mapme.biodiversity/issues/113))

- `.calc_active_fire_properties` now uses st_coordinates to retrieve
  locations of fires
  ([119](https://github.com/mapme-initiative/mapme.biodiversity/issues/119),
  [@DavisVaughan](https://github.com/DavisVaughan))

- tests for MacOS have been re-enabled
  ([131](https://github.com/mapme-initiative/mapme.biodiversity/issues/131))

- tests for downloading `nasa_srtm` resource are skipped because the SSL
  certificate of the online source has expired
  ([131](https://github.com/mapme-initiative/mapme.biodiversity/issues/131))

## mapme.biodiversity 0.2.1

CRAN release: 2022-09-09

### Bug fixes

- fixes a serious bug that occurred for tiled resources when multiple
  assets are within the same tile resulting in returning the same tile
  multiple times

### Internal

- tests to catch above mentioned bug have been introduced for tiled
  resources

## mapme.biodiversity 0.2.0

CRAN release: 2022-08-23

### Breaking changes

- extensive renaming of resources and indicators. These are handled
  gracefully until the next release (i.e. a warning is issued and names
  are replaced):

- resources:

- `treecover2000` -\> `gfw_treecover`

- `lossyear` -\> `gfw_lossyear`

- `greenhouse` -\> `gfw_emissions`

- `traveltime` -\> `nelson_et_al`

- `nasagrace` -\> `nasa_grace`

- `mintemperature` -\> `worldclim_min_temperature`

- `maxtemperature` -\> `worldclim_max_temperature`

- `precipitation` -\> `worldclim_precipitation`

- `ecoregions` -\> `teow`

- `mangrove` -\> `gmw`

- `srtmdem` -\> `nasa_srtm`

- indicators:

- `treecover` -\> `treecover_area`

- `emissions` -\> `treecoverloss_emissions`

- `treeloss` -\> `treecover_area_and_emissions`

- `chirpsprec` -\> `precipitation_chirps`

- `accessibility` -\> `traveltime`

- `popcount` -\> `population_count`

- `wctmin` -\> `temperature_min_wc`

- `wctmax` -\> `temperature_max_wc`

- `wcprec` -\> `precipitation_wc`

- `gmw` -\> `mangroves_area`

- `teow` -\> `ecoregion`

### New features

- new resource(s):
  - nasa_firms
- new indicator(s):
  - active_fire_properties
  - active_fire_counts

### Internal

- adapted download routine to GMW v3
  ([80](https://github.com/mapme-initiative/mapme.biodiversity/issues/80))
- removed data.table from imports

### Bug fixes

- fixing issue
  [84](https://github.com/mapme-initiative/mapme.biodiversity/issues/84)
  concerning intersection of tiled datasets
  ([86](https://github.com/mapme-initiative/mapme.biodiversity/issues/86),
  [@Jo-Schie](https://github.com/Jo-Schie))

## mapme.biodiversity 0.1.2

CRAN release: 2022-06-24

### Internal

- unit tests have been silenced in order to be more informative for
  reverse dependency checks
- checks for tile availability reactivated for SRTM
- fixed notes due to uninitialized variables in TEOW and biome
  indicators

## mapme.biodiversity 0.1.1

CRAN release: 2022-05-02

### Internal

- `init_portfolio()` now sets the `testing` attribute to FALSE by
  default.
- `get_<resource>()` functions now return filenames early if `testing`
  is set to TRUE.
- `calc_<indicator>()` examples now copy files to the R temporal
  directory and are wrapped in
  [`try()`](https://rdrr.io/r/base/try.html) to avoid errors/warnings on
  CRAN if an internet resource is not available.
- examples for
  [`calc_tri()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/tri.md)
  and
  [`calc_elevation()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/elevation.md)
  are now disabled on CRAN because of the responsiveness of CIGAR
  servers.

## mapme.biodiversity 0.1.0

CRAN release: 2022-04-27

### Breaking changes

- renamed ‘.assetid’ to ‘assetid’
  ([22](https://github.com/mapme-initiative/mapme.biodiversity/issues/22))

### New features

- None

### Internal

- ensures that tests and examples adhere to CRAN policies of only
  writing to the temporal directory
  ([22](https://github.com/mapme-initiative/mapme.biodiversity/issues/22)).

## mapme.biodiversity 0.0.1

CRAN release: 2022-04-19

### Initial release

- Added a `NEWS.md` file to track changes to the package.
- initial supported resources are:
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
- initial supported indicators are:
  - accessibility
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
- `init_portfolio()` is used to initialize a portfolio object. The input
  must be an sf object where all geometries are of type `POLYGON`
- users can request the download of one or more resources via
  [`get_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
- users can request the processing of an indicator via
  [`calc_indicators()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
- indicators are added to the portfolio object as nested list columns
- a processed portfolio object can be exported as a GeoPackage via
  [`write_portfolio()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md)
- a portfolio saved to disk as a GeoPackage can be read back into R via
  [`read_portfolio()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/portfolio.md).
  If users wish to download additional resources or calculate
  indicators, `init_portfolio()` has to be called again.
- Parallelization using multiple cores on the host machine is disabled
  on Windows
- on MacOS, the s2 engine for spherical geometric vector operations is
  disabled and lwgeom is used instead.

### Internal

- Introduced absolute URLS in userguide.Rmd pointing to the online
  documentation
  ([59](https://github.com/mapme-initiative/mapme.biodiversity/issues/59))
- tags added to all exported functions explaining what is the
  output/side effect
  ([59](https://github.com/mapme-initiative/mapme.biodiversity/issues/59))
- using requireNamespace() instead of installed.packages() to check if
  packages listed in SUGGEST are loadable
  ([58](https://github.com/mapme-initiative/mapme.biodiversity/issues/58))
