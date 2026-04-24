# Function to select processing engines

`check_engine()` checks if an extraction engine for zonal vector-raster
operations is supported by the backend.

`check_stats` checks if one or multiple statistics are supported for
zonal vector-raster extraction by the backend.

`select_engine` extracts zonal vector-raster statistics for supported
engine and for one or more statistics. Columns are named according to
the argument `name` plus the respective stat. Both `portfolio` and
`asset` modes are supported.

## Usage

``` r
check_engine(queried_engine)

check_stats(queried_stats)

select_engine(x, raster, stats, engine, name = NULL, mode = "asset")
```

## Arguments

- queried_engine:

  A character vector of length one indicating the engine to check for.

- queried_stats:

  A character vector with statistic names to be checked if they are
  supported by the backend

- x:

  An sf object representing a portfolio.

- raster:

  An terra SpatRaster from which values are to be extracted.

- stats:

  A character vector of statistics to aggregate the raster values with.

- engine:

  A character vector of length one specifying the engine to be used for
  the extraction.

- name:

  A character vector indicating the name to append to the columns names.

- mode:

  A character vector indicating in which mode to conduct the extraction
  (e.g. `asset`-wise or for the whole `portfolio` at once).

## Value

`check_engine()` returns the character of the queried engine, if
supported. Throws an error otherwise.

`check_stats` returns a character vector of supported statistics. Throws
an error if any of the queried statistics is not supported.

`select_engine` returns a tibble.
