% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial-utils.R
\name{spds_exists}
\alias{spds_exists}
\title{Check if a spatial data sets exists}
\usage{
spds_exists(path, oo = character(0), what = c("vector", "raster"))
}
\arguments{
\item{path}{A length 1 character vector with a GDAL readable file path.}

\item{oo}{Either a list or a character vector with opening options (-oo)
of the respective GDAL driver. A list must have equal length of the
input sources, a vector will be recycled.}

\item{what}{A character vector indicating if the resource is a vector or raster file.}
}
\value{
A logical, TRUE if the file exists, FALSE if it does not.
}
\description{
This function uses a file path readable by GDAL to check if it can query
it for information. Note, this should also work for remote files, e.g.
in an S3 bucket. You can use this function in your custom resource
function to query if a file is already present at the destination.
Note, that performance will be dependent on your connection to the
server. It can also be used for files on the local file system.
}
\examples{

# a vector resource
vec <- system.file("shape/nc.shp", package = "sf")
spds_exists(vec, what = "vector")

# a raster resource
ras <- system.file("ex/elev.tif", package = "terra")
spds_exists(ras, what = "raster")

# a non existing file
spds_exists("not-here.gpkg", what = "vector")

}
\keyword{utils}
