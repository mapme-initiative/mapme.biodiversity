# Portfolio methods for mapme.biodiversity

`mapme_options()` sets default options for mapme.biodiversity to control
the behaviour of downstream functions. Mainly, the output path as well
as the chunk size (in ha), can be set. Additionally, the verbosity can
be set and the path to a log directory can be controlled. Might be
extended by other options in the future.

`get_resources()` data sets required for the calculation of indicators
can be made available. The function supports the specification of
several resource functions. To determine the output path, temporary
directory and verbosity, the output of `mapme_options()` is used.

`calc_indicators()` calculates specific biodiversity indicators. A
requirement is that the resources that are mandatory inputs for the
requested indicators are available locally. Multiple indicators and
their respective additional arguments can be supplied.

This function reads and crops available resources to the extent of a
single asset. Specific resources can be queried. If not supplied (the
default), all available resources will be prepared.

## Usage

``` r
mapme_options(..., outdir, chunk_size, retries, verbose, log_dir)

get_resources(x, ...)

calc_indicators(x, ...)

prep_resources(
  x,
  avail_resources = NULL,
  resources = NULL,
  mode = c("portfolio", "asset")
)
```

## Arguments

- ...:

  One or more functions for resources/indicators

- outdir:

  A length one character indicating the output path.

- chunk_size:

  A numeric of length one giving the maximum chunk area in ha. Defaults
  to 100,000 ha. It refers to the area of an asset's bounding box. If it
  lies above the value `chunk_size`, splitting and chunking is
  considered. An asset will be processes as-is with an bounding box area
  below the specified value.

- retries:

  A numeric of length one indicating the number or re-tries the package
  should attempt to make a resource available. Defaults to 3.

- verbose:

  A logical, indicating if informative messages should be printed.

- log_dir:

  A character path pointing toward a GDAL-writable destination used to
  log erroneous assets. Defaults to NULL, meaning that erroneous assets
  will not be serialized to disk. If specified, a GPKG named
  `file.path(log_dir, paste0(Sys.Date(), "_mapme-error-assets.gpkg"))`
  will be created and appended to in case of erroneous assets.

- x:

  An `sf` object with features of type `"POLYGON"`

- avail_resources:

  A list object of available resources. If NULL (the default), the
  available resources will automatically be determined.

- resources:

  A character vector with the resources to be prepared. If it it is NULL
  (the default) all available resources will be prepared.

- mode:

  A character indicating the reading mode, e.g. either "portfolio" (the
  default) or "asset".

## Value

`mapme_options()` returns a list of options if no arguments are
specified. Otherwise sets matching arguments to new values in the
package's internal environment.

`get_resources()` is called for its side effect of making resources
available in the package environment. Returns `x`, invisibly.

`calc_indicators()` returns `x`, invisibly, with an additional nested
list column per requested indicator.

`prep_resources()` returns a list with prepared vector and raster
resources as `sf` and `SpatRaster`-objects.

## Examples

``` r
library(mapme.biodiversity)
mapme_options()
#> $outdir
#> [1] "/tmp/RtmpShs06Z/mapme-data"
#> 
#> $chunk_size
#> [1] 1e+08
#> 
#> $retries
#> [1] 3
#> 
#> $verbose
#> [1] FALSE
#> 
#> $log_dir
#> NULL
#> 
```
