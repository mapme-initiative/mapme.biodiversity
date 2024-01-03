
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
install.packages("mapme.biodiversity")
```

To install the development version, use the following command:

``` r
remotes::install_github("https://github.com/mapme-initiative/mapme.biodiversity")
```

## Usage example

`{mapme.biodiversity}` works by constructing a portfolio from an sf
object. Specific raster and vector resource matching the spatio-temporal
extent of the portfolio are made available locally. Once all required
resources are available, indicators can be calculated individually for
each asset in the portfolio.

To list all available resources and indicators run:

``` r
library(mapme.biodiversity)
library(sf)
```

    ## Linking to GEOS 3.11.1, GDAL 3.8.2, PROJ 9.1.1; sf_use_s2() is TRUE

``` r
resources <- names(available_resources())
indicators <- names(available_indicators())
cat(sprintf(
  "Supported resources:\n- %s\n\nSupported indicators:\n- %s",
  paste(resources, collapse = "\n- "),
  paste(indicators, collapse = "\n- ")
))
```

    ## Supported resources:
    ## - chirps
    ## - esalandcover
    ## - fritz_et_al
    ## - gfw_emissions
    ## - gfw_lossyear
    ## - gfw_treecover
    ## - gmw
    ## - nasa_firms
    ## - nasa_grace
    ## - nasa_srtm
    ## - nelson_et_al
    ## - soilgrids
    ## - teow
    ## - ucdp_ged
    ## - worldclim_max_temperature
    ## - worldclim_min_temperature
    ## - worldclim_precipitation
    ## - worldpop
    ## 
    ## Supported indicators:
    ## - active_fire_counts
    ## - active_fire_properties
    ## - biome
    ## - deforestation_drivers
    ## - drought_indicator
    ## - ecoregion
    ## - elevation
    ## - fatalities
    ## - landcover
    ## - mangroves_area
    ## - population_count
    ## - precipitation_chirps
    ## - precipitation_wc
    ## - soilproperties
    ## - temperature_max_wc
    ## - temperature_min_wc
    ## - traveltime
    ## - treecover_area
    ## - treecover_area_and_emissions
    ## - treecoverloss_emissions
    ## - tri

Once you have decided on an indicator you are interested in, you can
initialize a biodiversity portfolio by using an sf-object that only
contains geometries of type `POLYGON` via the `init_portfolio()`
function call. This will set some important information that is needed
further down the processing chain. We can then request the download of a
resource that is required to calculate specific indicators. Once the
indicator has been calculated for individually for all assets in a
portfolio, the data is returned as a nested list column to the original
object.

``` r
(system.file("extdata", "sierra_de_neiba_478140_2.gpkg", package = "mapme.biodiversity") %>%
  sf::read_sf() %>%
  init_portfolio(
    years = 2016:2017,
    outdir = system.file("res", package = "mapme.biodiversity"),
    tmpdir = system.file("tmp", package = "mapme.biodiversity"),
    verbose = FALSE
  ) %>%
  get_resources(
    resources = c("gfw_treecover", "gfw_lossyear", "gfw_emissions"),
    vers_treecover = "GFC-2020-v1.8", vers_lossyear = "GFC-2020-v1.8"
  ) %>%
  calc_indicators("treecover_area_and_emissions", min_size = 1, min_cover = 30) %>%
  tidyr::unnest(treecover_area_and_emissions))
```

    ## Simple feature collection with 2 features and 8 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 2 × 9
    ##   WDPAID NAME            DESIG_ENG     ISO3  assetid years emissions treecover
    ##    <dbl> <chr>           <chr>         <chr>   <int> <int>     <dbl>     <dbl>
    ## 1 478140 Sierra de Neiba National Park DOM         1  2016      2832     2357.
    ## 2 478140 Sierra de Neiba National Park DOM         1  2017      3468     2345.
    ## # ℹ 1 more variable: geom <POLYGON [°]>

## A note on parallel computing

{mapme.biodiversity} follows the parallel computing paradigm of the
{[future](https://cran.r-project.org/package=future)} package. That
means that you as a user are in the control if and how you would like to
set up parallel processing. Currently, {mapme.biodiversity} supports
parallel processing on the asset level of the `calc_indicators()`
function only. We also currently assume that parallel processing is done
on the cores of a single machine. In future developments, we would like
to support distributed processing. If you are working on a distributed
use-cases, please contact the developers, e.g. via the [discussion
board](https://github.com/mapme-initiative/mapme.biodiversity/discussions)
or mail.

To process 6 assets in parallel and report a progress bar you will have
to set up the following in your code:

``` r
library(future)
library(progressr)

plan(multisession, workers = 6) # set up parallel plan

with_progress({
  portfolio <- calc_indicators(
    portfolio,
    "treecover_area_and_emissions",
    min_size = 1,
    min_cover = 30
  )
})

plan(sequential) # close child processes
```

Note, that the above code uses `future::multisession()` as the parallel
backend. This backend will resolve the calculation in multiple
background R sessions. You should use that backend if you are operating
on Windows, using RStudio or otherwise are not sure about which backend
to use. In case you are operating on a system that allows process
forking and are *not* using RStudio, consider using
`future::multicore()` for more efficient parallel processing.

Head over to the [online
documentation](https://mapme-initiative.github.io/mapme.biodiversity/index.html)
find more detailed information about the package.
