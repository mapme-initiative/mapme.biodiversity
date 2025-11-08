# Check if a spatial data sets exists

This function uses a file path readable by GDAL to check if it can query
it for information. Note, this should also work for remote files, e.g.
in an S3 bucket. You can use this function in your custom resource
function to query if a file is already present at the destination. Note,
that performance will be dependent on your connection to the server. It
can also be used for files on the local file system.

## Usage

``` r
spds_exists(path, oo = character(0), what = c("vector", "raster"))
```

## Arguments

- path:

  A length 1 character vector with a GDAL readable file path.

- oo:

  Either a list or a character vector with opening options (-oo) of the
  respective GDAL driver. A list must have equal length of the input
  sources, a vector will be recycled.

- what:

  A character vector indicating if the resource is a vector or raster
  file.

## Value

A logical, TRUE if the file exists, FALSE if it does not.

## Examples

``` r
# a vector resource
vec <- system.file("shape/nc.shp", package = "sf")
spds_exists(vec, what = "vector")
#> [1] TRUE

# a raster resource
ras <- system.file("ex/elev.tif", package = "terra")
spds_exists(ras, what = "raster")
#> [1] TRUE

# a non existing file
spds_exists("not-here.gpkg", what = "vector")
#> [1] FALSE
```
