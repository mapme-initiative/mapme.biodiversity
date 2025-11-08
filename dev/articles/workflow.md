# Workflow

The most important steps of the processing workflow of
[mapme.biodiversity](https://mapme-initiative.github.io/mapme.biodiversity/)
are described here. A typical workflow consists of the following steps:

- initialization of a portfolio object from an
  [sf](https://r-spatial.github.io/sf/) object together with
  portfolio-wide parameters set by
  [`mapme_options()`](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/mapme.md)
- identify the required resources needed for the indicators you want to
  calculate
- download one or more resources that are required for your desired
  indicators
- automated calculation of the indicators for all assets within a
  portfolio
- then, you are free to conduct your statistical analysis within R, or,
- you can export the portfolio as a GeoPackage or other spatial data
  formats to share with others or use it with other Geo-spatial software
  (e.g. QGIS)
