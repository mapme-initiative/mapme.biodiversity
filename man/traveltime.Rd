% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_traveltime.R
\docType{data}
\name{traveltime}
\alias{traveltime}
\title{Calculate accessibility statistics}
\format{
A tibble with a column for accessibility statistics (in minutes)
}
\description{
Accessibility is the ease with which larger cities can be reached from a
certain location. This function allows to efficiently calculate accessibility
statistics (i.e. travel time to nearby major cities) for polygons. For each
polygon, the desired statistic/s (mean, median or sd) is/are returned.
The required resources for this indicator are:
\itemize{
\item \link{nelson_et_al}
}
}
\details{
The following arguments can be set:
\describe{
\item{stats_accessibility}{Function to be applied to compute statistics for polygons either
one or multiple inputs as character. Supported statistics are: "mean",
"median", "sd", "min", "max", "sum" "var".}
\item{engine}{The preferred processing functions from either one of "zonal",
"extract" or "exactextract" as character.}
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
    years = 2022,
    outdir = outdir,
    tmpdir = tempdir(),
    verbose = FALSE
  ) \%>\%
  get_resources("nelson_et_al",
    range_traveltime = c("5k_10k", "100k_200k", "500k_1mio", "1mio_5mio")
  ) \%>\%
  calc_indicators("traveltime", stats_accessibility = c("min", "max"), engine = "extract") \%>\%
  tidyr::unnest(traveltime)

aoi
}
}
\keyword{indicator}
