# Portfolio methods

`write_portfolio()` writes a processed biodiversity portfolio to disk.
Portfolio data will only be serialized to disk as a GeoPackage including
two tables: `metadata` and `indicators`. The `metadata` tables includes,
among other simple variables the geometries and a primary key called
`assetid`. The 'indicators' tables includes the foreign key `assetid`, a
column called `indicator` giving the name of the original indicator as
well as the standard indicator columns `datetime`, `variable`, `unit`,
and `value`. For convenience, use `read_portfolio()` to read such a
portfolio GeoPackage back into R.

`portfolio_long()` transforms a portfolio to long-format, potentially
dropping geometries in the process.

`portfolio_wide()` transforms a portfolio to wide-format, potentially
dropping geometries in the process.

## Usage

``` r
write_portfolio(x, dsn, ...)

read_portfolio(src, ...)

portfolio_long(x, indicators = NULL, drop_geoms = FALSE)

portfolio_wide(x, indicators = NULL, drop_geoms = FALSE)
```

## Arguments

- x:

  A portfolio object processed with `mapme.biodiversity`.

- dsn:

  A file path for the output file (must end with `gpkg`).

- ...:

  Additional arguments supplied to
  [`write_sf()`](https://r-spatial.github.io/sf/reference/st_write.html)
  or
  [`read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html)

- src:

  A character vector pointing to a GeoPackage that has been previously
  written to disk via `write_portfolio()`

- indicators:

  If NULL (the default), all indicator columns will be detected and
  transformed automatically. If a character vector is supplied, only
  those indicators will be transformed.

- drop_geoms:

  A logical, indicating if geometries should be dropped.

## Value

`write_portfolio()` returns `dsn`, invisibly.

`read_portfolio()` returns an `sf` object object with nested list
columns for every indicator found in the GeoPackage source file.

`portfolio_long()` returns the portfolio object in long-format.

`portfolio_wide()` returns the portfolio object in wide-format.
