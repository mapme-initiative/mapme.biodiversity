% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_active_fire_counts.R
\docType{data}
\name{active_fire_counts}
\alias{active_fire_counts}
\title{Calculate active fire counts based on NASA FIRMS polygons}
\format{
A tibble with a column for number of fire events per year and instrument.
}
\description{
This function allows to efficiently calculate the number of fire
events occurred in the region of interest from the NASA FIRMS active
fire polygon datasets. For each polygon, the fire event counts for
the desired year is returned.
The required resources for this indicator are:
\itemize{
\item \link{nasa_firms}
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

aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) \%>\%
  read_sf() \%>\%
  init_portfolio(
    years = 2021,
    outdir = outdir,
    tmpdir = tempdir(),
    verbose = FALSE
  ) \%>\%
  get_resources("nasa_firms", instrument = "VIIRS") \%>\%
  calc_indicators("active_fire_counts") \%>\%
  tidyr::unnest(active_fire_counts)

aoi
}
}
\keyword{indicator}
