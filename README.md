
<!-- badges: start -->

[![R-CMD-check](https://github.com/mapme-initiative/mapme.biodiversity/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/mapme-initiative/mapme.biodiversity/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/mapme-initiative/mapme.biodiversity/master.svg)](https://app.codecov.io/github/mapme-initiative/mapme.biodiversity?branch=main)
[![CRAN
status](https://badges.cranchecks.info/worst/mapme.biodiversity.svg)](https://cran.r-project.org/web/checks/check_results_mapme.biodiversity.html)
[![CRAN
version](https://www.r-pkg.org/badges/version/mapme.biodiversity)](https://CRAN.R-project.org/package=mapme.biodiversity)
[![License](https://img.shields.io/badge/License-GPL%20(%3E=3)-brightgreen.svg?style=flat)](https://choosealicense.com/licenses/gpl-3.0/)

<!-- badges: end -->

# mapme.biodiversity <img src="man/figures/logo.png" align="right" height="110"/>

> \[!IMPORTANT\]  
> `{mapme.biodiversity}` is currently experiencing major changes to its
> user-interface and overall functionality. Please visit the
> [announcement
> issue](https://github.com/mapme-initiative/mapme.biodiversity/issues/240)
> to learn more about these changes.

## About

Biodiversity areas, especially primary forests, provide multiple
ecosystem services for the local population and the planet as a whole.
The rapid expansion of human land use into natural ecosystems and the
impacts of the global climate crisis put natural ecosystems and the
global biodiversity under threat.

The mapme.biodiversity package helps to analyse a number of biodiversity
related indicators and biodiversity threats based on freely available
geodata-sources such as the Global Forest Watch. It supports
computational efficient routines and heavy parallel computing in
cloud-infrastructures such as AWS or Microsoft Azure using in the
statistical programming language R. The package allows for the analysis
of global biodiversity portfolios with a thousand or millions of AOIs
which is normally only possible on dedicated platforms such as the
Google Earth Engine. It provides the possibility to e.g. analyse the
World Database of Protected Areas (WDPA) for a number of relevant
indicators. The primary use case of this package is to support
scientific analysis and data science for individuals and organizations
who seek to preserve the planet biodiversity. Its development is funded
by the German Development Bank KfW.

## Installation

The package and its dependencies can be installed from CRAN via:

``` r
install.packages("mapme.biodiversity", dependencies = TRUE)
```

To install the development version, use the following command:

``` r
remotes::install_github("https://github.com/mapme-initiative/mapme.biodiversity", dependencies = TRUE)
```

## Available resources and indicators

Below is a list of the resources currently supported by
`mapme.biodiversity`.

| name                             | description                                                                                          | licence                                                                  |
|:---------------------------------|:-----------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------|
| chirps                           | Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)                              | CC - unknown                                                             |
| esalandcover                     | Copernicus Land Monitoring Service (CLMS) 100 meter land cover product                               | CC-BY 4.0                                                                |
| fritz_et_al                      | Drivers of deforestation in the tropics                                                              | CC-BY 4.0                                                                |
| gfw_emissions                    | Global Forest Watch - CO2 Emssions caused by forest cover loss                                       | CC-BY 4.0                                                                |
| gfw_lossyear                     | Global Forest Watch - Year of forest cover loss occurence                                            | CC-BY 4.0                                                                |
| gfw_treecover                    | Global Forest Watch - Percentage of canopy closure in 2000                                           | CC-BY 4.0                                                                |
| global_surface_water_change      | Global Surface Water - Change of water occurrence intensity                                          | <https://www.copernicus.eu/en/access-data>                               |
| global_surface_water_occurrence  | Global Surface Water - Percentage of water occurrence                                                | <https://www.copernicus.eu/en/access-data>                               |
| global_surface_water_recurrence  | Global Surface Water - Percentage of water recurrence                                                | <https://www.copernicus.eu/en/access-data>                               |
| global_surface_water_seasonality | Global Surface Water - Seasonality of water occurrrence                                              | <https://www.copernicus.eu/en/access-data>                               |
| global_surface_water_transitions | Global Surface Water - Transition classes                                                            | <https://www.copernicus.eu/en/access-data>                               |
| gmw                              | Global Mangrove Watch - Vector data of mangrove extent                                               | CC BY 4.0                                                                |
| nasa_firms                       | NASA Fire Information for Resource Management System (FIRMS) - Global fire map data archive          | <https://www.earthdata.nasa.gov/learn/find-data/near-real-time/citation> |
| nasa_grace                       | NASA Gravity Recovery And Climate Experiment (GRACE) - Measurments of Earth’s mass and water changes | <https://nasagrace.unl.edu/About.aspx>                                   |
| nasa_srtm                        | NASA Shuttle Radar Topography Mission (SRTM) Digital Elevation Model (DEM)                           | <https://lpdaac.usgs.gov/data/data-citation-and-policies/>               |
| nelson_et_al                     | Global maps of traveltime to cities                                                                  | CC-BY 4.0                                                                |
| soilgrids                        | ISRIC - Modelled global soil property layers                                                         | CC-BY 4.0                                                                |
| teow                             | Terrestrial Ecosystems of the World (TEOW) from WWF-US                                               | unknown                                                                  |
| ucdp_ged                         | UCDP Georeferenced Event Dataset (UCDP GED)                                                          | CC-BY 4.0                                                                |
| worldclim_max_temperature        | WorldClim - Monthly maximum temperature 2000 - 2018                                                  | <https://www.worldclim.org/about.html>                                   |
| worldclim_min_temperature        | WorldClim - Monthly minimum temperature 2000 - 2018                                                  | <https://www.worldclim.org/about.html>                                   |
| worldclim_precipitation          | WorldClim - Monthly precipitation 2000 - 2018                                                        | <https://www.worldclim.org/about.html>                                   |
| worldpop                         | WorldPop - Unconstrained Global Mosaics 2000 - 2020                                                  | CC-BY 4.0                                                                |

Next, is a list of supported indicators.

| name                         | description                                                                    |
|:-----------------------------|:-------------------------------------------------------------------------------|
| active_fire_counts           | Number of detected fires by NASA FIRMS                                         |
| biome                        | Areal statistics of biomes from TEOW                                           |
| deforestation_drivers        | Areal statistics of deforestation drivers                                      |
| drought_indicator            | Relative wetness statistics based on NASA GRACE                                |
| ecoregion                    | Areal statstics of ecoregions based on TEOW                                    |
| elevation                    | Statistics of elevation based on NASA SRTM                                     |
| fatalities                   | Number of fatalities by group of conflict based on UCDP GED                    |
| gsw_change                   | Statistics of the surface water change layer by JRC                            |
| gsw_occurrence               | Areal statistic of surface water based on occurrence threshold                 |
| gsw_recurrence               | Areal statistic of surface water based on reccurence threshold                 |
| gsw_seasonality              | Areal statistic of surface water by seasonality                                |
| gsw_transitions              | Areal statistics of surface water grouped by transition class                  |
| landcover                    | Areal statistics grouped by landcover class                                    |
| mangroves_area               | Area covered by mangroves                                                      |
| population_count             | Statistic of population counts                                                 |
| precipitation_chirps         | Statistics of CHIRPS precipitation layer                                       |
| precipitation_wc             | Statistics of WorldClim precipitation layer                                    |
| soilproperties               | Statistics of SoilGrids layers                                                 |
| temperature_max_wc           | Statistics of WorldClim maximum temperature layer                              |
| temperature_min_wc           | Statistics of WorldClim minimum temperature layer                              |
| traveltime                   | Statistics of traveltime to the clostes city grouped by city category          |
| treecover_area               | Area of forest cover by year                                                   |
| treecover_area_and_emissions | Area of forest cover and greenhouse gas emssions caused by forest loss by year |
| treecoverloss_emissions      | Greenouse gas emissions cause by forest loss by year                           |
| tri                          | Statistics of terrain rudgedness index based on NASA SRTM DEM                  |

## Usage example

`{mapme.biodiversity}` works by constructing a portfolio from an sf
object. Specific raster and vector resource matching the spatio-temporal
extent of the portfolio are made available locally. Once all required
resources are available, indicators can be calculated individually for
each asset in the portfolio.

``` r
library(mapme.biodiversity)
library(sf)
```

    ## Linking to GEOS 3.12.1, GDAL 3.8.2, PROJ 9.3.1; sf_use_s2() is TRUE

Once you have decided on an indicator you are interested in, you can
start by making the required resource available for your portfolio.
Using `mapme_options()` you can set an output directory, control the
maximum size of polygons before they are chunked into smaller parts, and
control the verbosity of the package.

A portfolio is represented by an sf-object. It is required for the
object to only contain geometries of type `POLYGON` and `MULTIPOLYGON`
as assets. We can request the download of a resource for the spatial
extent of our portfolio by using the `get_resources()` function. We
simply supply our portfolio and one or more resource functions. Once the
resources were made available, we can query the calculation of an
indicator by using the `calc_indicators()` function. This function also
expects the portfolio as input and one or more indicator functions. Once
the indicator has been calculated for all assets in a portfolio, the
data is returned as a nested list column to the original portfolio
object. The output of each indicator is standardized to common format,
consisting of a tibble with columns `datetime`, `variable`, `unit`, and
`value`. We can transform the the data to long format by using
`portfolio_long()`.

``` r
mapme_options(
  outdir = system.file("res", package = "mapme.biodiversity"),
  chunk_size = 1e6, # in ha
  verbose = FALSE
)

aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg", package = "mapme.biodiversity") %>%
  sf::read_sf() %>%
  get_resources(
    get_gfw_treecover(version = "GFC-2023-v1.11"),
    get_gfw_lossyear(version = "GFC-2023-v1.11"),
    get_gfw_emissions()
  ) %>%
  calc_indicators(calc_treecover_area_and_emissions(years = 2016:2017, min_size = 1, min_cover = 30)) %>%
  portfolio_long()

aoi
```

    ## Simple feature collection with 4 features and 10 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 4 × 11
    ##   WDPAID NAME  DESIG_ENG ISO3  assetid indicator datetime   variable unit  value
    ##    <dbl> <chr> <chr>     <chr>   <int> <chr>     <date>     <chr>    <chr> <dbl>
    ## 1 478140 Sier… National… DOM         1 treecove… 2016-01-01 emissio… Mg    4296.
    ## 2 478140 Sier… National… DOM         1 treecove… 2016-01-01 treecov… ha    2370.
    ## 3 478140 Sier… National… DOM         1 treecove… 2017-01-01 emissio… Mg    4970.
    ## 4 478140 Sier… National… DOM         1 treecove… 2017-01-01 treecov… ha    2358.
    ## # ℹ 1 more variable: geom <POLYGON [°]>

## A note on parallel computing

`{mapme.biodiversity}` follows the parallel computing paradigm of the
[`{future}`](https://cran.r-project.org/package=future) package. That
means that you as a user are in the control if and how you would like to
set up parallel processing. Since `{mapme.biodiversity} v0.7` supports
parallel processing on a nested-level. The outer level applies parallel
processing to the assets in a portfolio, the inner level to potential
chunks for polygons that are larger than the specified chunks size or
the single components of assets of type `MULTIPOLYGON`.

The maximum chunk size is specified in hectares via `mapme_options()`
and defaults to 100,000 ha. Polygons larger than this threshold will be
split into chunks of roughly the same size.

Fine-control of parallel processing is given by using future topologies
(find more information
[here](https://cran.r-project.org/package=future/vignettes/future-3-topologies.html)).
To process all assets sequentially, but allow to spawn up to 4 workers
to process chunks in parallel you might specify:

``` r
library(future)
plan(list(sequential, tweak(cluster, workers = 6)))
```

As another example, with the code below one would apply parallel
processing of 2 assets, with each having 4 workers available to process
chunks, thus requiring a total of 8 available cores on the host machine.
Be sure to not request more workers than available on your machine.

``` r
library(progressr)

plan(list(tweak(cluster, workers = 2), tweak(cluster, workers = 4)))

with_progress({
  aoi <- calc_indicators(
    aoi,
    calc_treecover_area_and_emissions(
      min_size = 1,
      min_cover = 30
    )
  )
})

plan(sequential) # close child processes
```

Head over to the [online
documentation](https://mapme-initiative.github.io/mapme.biodiversity/index.html)
find more detailed information about the package.
