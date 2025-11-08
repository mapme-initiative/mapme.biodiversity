# Helper to create a grid of regular resolution and CRS

Use this function to create a regular grid in a custom CRS. This is used
e.g. to create the tile grid for Global Forest Watch in order to
retrieve the intersecting tiles with a given portfolio.

## Usage

``` r
make_global_grid(
  xmin = -180,
  xmax = 170,
  dx = 10,
  ymin = -50,
  ymax = 80,
  dy = 10,
  proj = NULL
)
```

## Arguments

- xmin:

  minimum longitude value (E/W)

- xmax:

  maximum longitude value (E/W)

- dx:

  difference in longitude value per grid

- ymin:

  minimum latitude value (S/N)

- ymax:

  maximum latitude value (E/W)

- dy:

  difference in latitude value per grid

- proj:

  projection system

## Value

An sf object with a defined grid.
