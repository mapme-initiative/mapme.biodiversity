
<!-- badges: start -->

[![R-CMD-check](https://github.com/mapme-initiative/mapme.biodiversity/workflows/R-CMD-check/badge.svg)](https://github.com/mapme-initiative/mapme.biodiversity/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/mapme-initiative/mapme.biodiversity/master.svg)](https://codecov.io/github/mapme-initiative/mapme.biodiversity?branch=master)
<!-- badges: end -->

# mapme.biodiversity <img src="man/figures/logo.png" align="right" height="110"/>

## About

Biodiversity areas, especially primary forests, provide multiple
ecosystem services for the local population and the planet as a whole.
The rapid expansion of human landuse into natural ecosystems and the
impacts of the global climate crisis put natural ecosystems and the
global biodiversity under threat.

The mapme.biodiversity package helps to analyse a number of biodiversity
related indicators and biodiversity threats based on freely available
geodata-sources such as the Global Forest Watch. It supports
computational efficient routines and heavy parallelization in
cloud-infrastructures such as AWS or AZURE using in the statistical
programming language R. The package allows for the analysis of global
biodiversity portfolios with a thousand or millions of AOIs which is
normally only possible on dedicated platforms such as the Google Earth
Engine. It provides the possibility to e.g. analyze the World Database
of Protected Areas (WDPA) for a number of relevant indicators. The
primary use case of this package is to support scientific analysis and
data science for individuals and organizations who seek to preserve the
planets biodiversity. It’s development is funded by the German
Development Bank KfW.

## Installation

The package and its dependencies can be installed via:

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

    ## Linking to GEOS 3.10.1, GDAL 3.4.0, PROJ 8.2.0; sf_use_s2() is TRUE

``` r
resources <- names(available_resources())
indicators <- names(available_indicators())
cat(sprintf("Supported resources:\n- %s\n\nSupported indicators:\n- %s",
            paste(resources, collapse = "\n- "),
            paste(indicators, collapse = "\n- ")))
```

    ## Supported resources:
    ## - chirps
    ## - ecoregions
    ## - esalandcover
    ## - greenhouse
    ## - lossyear
    ## - mangrove
    ## - maxtemperature
    ## - mintemperature
    ## - nasagrace
    ## - precipitation
    ## - soilgrids
    ## - srtmdem
    ## - traveltime
    ## - treecover2000
    ## - worldpop
    ## 
    ## Supported indicators:
    ## - accessibility
    ## - biome
    ## - chirpsprec
    ## - drought_indicator
    ## - elevation
    ## - emissions
    ## - gmw
    ## - landcover
    ## - popcount
    ## - soilproperties
    ## - teow
    ## - treecover
    ## - treeloss
    ## - tri
    ## - wcprec
    ## - wctmax
    ## - wctmin

Once you have decided on an indicator you are interested in, you can
initialize a biodiversity portfolio by using an sf-object that only
contains geometries of type `POLYGON` via the `init_portfolio()`
function call. This will set some important information that is needed
further down the processing chain. We can then request the download of a
resources that is required to calculate specific indicators. Once the
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
      add_resources = FALSE,
      cores = 1,
      verbose = FALSE
    ) %>%
    get_resources(
      resources = c("treecover2000", "lossyear", "greenhouse"),
      vers_treecover = "GFC-2020-v1.8", vers_lossyear = "GFC-2020-v1.8"
    ) %>%
    calc_indicators("treeloss", min_size = 1, min_cover = 30) %>%
    tidyr::unnest(treeloss))
```

    ## Simple feature collection with 2 features and 8 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 2 × 9
    ##   WDPAID NAME            DESIG_ENG     ISO3  .assetid years emissions treecover
    ##    <dbl> <chr>           <chr>         <chr>    <int> <int>     <dbl>     <dbl>
    ## 1 478140 Sierra de Neiba National Park DOM          1  2016      2832     2357.
    ## 2 478140 Sierra de Neiba National Park DOM          1  2017      3468     2345.
    ## # … with 1 more variable: geom <POLYGON [°]>

Head over to the [online
documentation](https://mapme-initiative.github.io/mapme.biodiversity/index.html)
find more detailed information about the package.
