# Installation

This section describes how to install the
[mapme.biodiversity](https://mapme-initiative.github.io/mapme.biodiversity/)
package. Since the package handles spatial data, we assume that you have
a working installation of [GDAL](https://gdal.org/index.html) and
[PROJ](https://proj.org/) on your system. Our recommendation is to
follow the [sf](https://r-spatial.github.io/sf/) installation
instructions.

While the package supports sequentially downloading all supported
resources, we include the option to provide a path to an
[aria2](https://aria2.github.io/) executable to speed up downloads based
on parallel processing. We expect that the path to the executable binary
is known to the user and we test if it is actually executable.

Given that the above requirements are met, installing the
[mapme.biodiversity](https://mapme-initiative.github.io/mapme.biodiversity/)
current CRAN release is then achieved through the following function
call:

``` r
install.packages("mapme.biodiversity")
```

You can install the latest development version from GitHub via the
following call:

``` r
remotes::install_github("mapme-initiative/mapme.biodiversity")
```

The installation process will install potential missing R dependencies.
After the successful installation, you are ready to use the package’s
functionality. Simply load the package for your R session with the
library call:

``` r
library(mapme.biodiversity)
```
