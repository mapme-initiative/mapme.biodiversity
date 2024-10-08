% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_slope.R
\name{slope}
\alias{slope}
\alias{calc_slope}
\title{Calculate slope statistics}
\usage{
calc_slope(engine = "exactextract", stats = "mean")
}
\arguments{
\item{engine}{The preferred processing function from either one of "zonal",
"extract" or "exactextract" as a character string.}

\item{stats}{Function to be applied to compute statistics for polygons.
Accepts either a single string or a vector of strings, such as "mean",
"median", "sd", "min", "max", "sum", or "var".}
}
\value{
A function that returns an indicator tibble with specified slope
statistics as variables and corresponding values (in degrees).
}
\description{
This function allows to calculate slope statistics for
polygons. For each polygon, the desired statistic(s)
are returned.
}
\details{
The required resource for this indicator is:
\itemize{
\item \link{nasa_srtm}
}
}
\examples{
\dontshow{
mapme.biodiversity:::.copy_resource_dir(file.path(tempdir(), "mapme-data"))
}
\dontrun{
library(sf)
library(mapme.biodiversity)

outdir <- file.path(tempdir(), "mapme-data")
dir.create(outdir, showWarnings = FALSE)

mapme_options(
  outdir = outdir,
  verbose = FALSE
)

aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) \%>\%
  read_sf() \%>\%
  get_resources(get_nasa_srtm()) \%>\%
  calc_indicators(
    calc_slope(stats = c("mean", "median", "sd", "var"), engine = "extract")
  ) \%>\%
  portfolio_long()

aoi
}
}
\keyword{indicator}
