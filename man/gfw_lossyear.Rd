% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gfw_lossyear.R
\docType{data}
\name{gfw_lossyear}
\alias{gfw_lossyear}
\title{Year of forest loss occurrence}
\format{
A global tiled raster resource available for all land areas.
}
\source{
\url{https://data.globalforestwatch.org/documents/tree-cover-loss/explore}
}
\description{
This resource is part of the publication by Hansen et al. (2013)
"High-Resolution Global Maps of 21st-Century Forest Cover Change". It
represents "Forest loss during the period 2000–2021, defined as a
stand-replacement disturbance, or a change from a forest to non-forest state.
Encoded as either 0 (no loss) or else a value in the range 1–20, representing
loss detected primarily in the year 2001–2021, respectively." Due to changes
in the satellites products used in the compilation of the tree loss product,
results before the year 2011 and afterwards are not directly comparable
until reprocessing has finished. Users should be aware of this limitation,
especially when the timeframe of the analysis spans over the two periods
delimited by the year 2011.
}
\details{
The following argument can be set:
\describe{
\item{vers_lossyear}{The version of the dataset to download. Defaults to
"GFC-2022-v1.10". Check \code{mapme.biodiversity:::.available_gfw_versions()}
to get a list of available versions}
}
}
\references{
Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A.
Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R.
Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G.
Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover
Change.” Science 342 (15 November): 850–53.
}
\keyword{resource}
