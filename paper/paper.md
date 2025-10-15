---
title: "mapme.biodiversity: Efficient Monitoring of Global Biodiversity Portfolios"
tags:
  - R
  - conservation research
  - protected areas
  - spatial analysis
authors:
  - name: Darius A. Görgen
    orcid: 0009-0008-5503-7704
    affiliation: 1
  - name: Zivan Karaman
    orcid: 0009-0008-5503-7704
    affiliation: 2
  - name: Andreas Petutschnig
    orcid: 0000-0001-5029-2425
    affiliation: 3
  - name: Florent Bédécarrats
    orcid: 0000-0003-1001-5540
    affiliation: 4
  - name: Johannes Schielein
    orcid: 0000-0001-6286-0688
    affiliation: 5
affiliations:
 - name: Department of Landscape Ecology, University of Münster, Germany
   index: 1
 - name: Independent consultant, France
   index: 2
 - name: Department of Geoinformatics, University of Salzburg, Austria
   index: 3
 - name: UMI SOURCE, Université Paris-Saclay, UVSQ, IRD, Guyancourt, France
   index: 4
 - name: KfW Development Bank, Bonn, Germany
   index: 5
date: "09 October 2025"
output: rticles::joss_article
journal: JOSS
bibliography: paper.bib
link-citations: true
year: 2025
software_repository_url: www.github.com/mapme-initiative/mapme.biodiversity
submitted: unsubmitted
published: unpublished
---

# Summary

The \texttt{mapme.biodiversity} \texttt{R} package provides an open and reproducible framework for computing biodiversity-related indicators from multiple global geospatial datasets. It streamlines access, preprocessing, and analysis of spatial data to produce standardized, area-based metrics that can be used for conservation monitoring, policy evaluation, and research. The package integrates existing R tools for spatial data handling and parallel processing, supports both local and cloud-based workflows, and allows users to extend functionality with new datasets and indicators. By enabling reproducible and scalable analyses across diverse regions and themes, \texttt{mapme.biodiversity} helps bridge the gap between raw geospatial information and decision-relevant biodiversity indicators.

# Statement of need

To prevent biodiversity loss at scale, conservation researchers and practitioners require area-based indicators derived from diverse spatial datasets. Monitoring efforts often involve sets of intervention areas that must be tracked consistently over time. However, relevant data sources are dispersed across multiple repositories and platforms, each with distinct access protocols, formats, and documentation standards.

Most existing tools focus on a specific data source or domain, offering limited interoperability and requiring users to learn multiple interfaces. This fragmentation imposes a high cognitive and technical burden, especially on users who are not remote sensing specialists but need spatial data for research, monitoring, or policy evaluation.

The \texttt{mapme.biodiversity} \texttt{R} package provides a unified interface to access and process a wide range of spatial datasets relevant to conservation and environmental management. It enables users to derive standardized, area-based indicators at scale, supports reproducible workflows in \texttt{R}, and facilitates integration with other tools via export to standard spatial formats.

Originally developed for conservation monitoring, the package has also found applications in adjacent domains such as agriculture, rural development, food security, and infrastructure planning. It offers a transparent and auditable alternative to proprietary platform-as-a-service (PaaS) solutions like Google Earth Engine or Microsoft Planetary Computer, which may be inaccessible to users in low-connectivity settings, non-academic institutions, or those requiring long-term reproducibility not guaranteed by commercial platforms.

# Applications

As an open-source package, the full range of \texttt{mapme.biodiversity} applications is difficult to track. However, download statistics indicate substantial usage, with 19,599 downloads from CRAN between January 2019 and September 2025. The package is actively used by several institutions for both operational and research purposes.

At the Kreditanstalt für Wiederaufbau (KfW) and the Agence Française de Développement (AFD) -- the German and French public development banks for international development -- it is used for internal impact evaluations and reporting on funded conservation and development programs. At the French National Research Institute for Sustainable Development (IRD), it is used in research on policy evaluation and in projects aimed at strengthening research capacity in the Global South, including ongoing work in Madagascar and Senegal.

# Comparison with other software packages

Different software tools address related needs in conservation monitoring and biodiversity assessment. The Digital Observatory for Protected Areas (\texttt{DOPA}) provides a centralized, server-based system developed by the European Commission to compute global protected-area indicators through standardized, automated workflows [@juffe-bignoli_delivering_2024]. In contrast, mapme.biodiversity offers an R-native, decentralized framework that enables users to reproduce similar area-based analyses locally, adapt them to specific contexts, and extend them with additional datasets. Both share the goal of consistent and transparent indicator production, but differ in scale, accessibility, and infrastructure dependency.

The \texttt{wdpar} \texttt{R} package facilitates access to and cleaning of spatial data from the World Database on Protected Areas [@hanson_wdpar_2022]. \texttt{mapme.biodiversity} is commonly used alongside \texttt{wdpar} to compute diverse environmental indicators from multiple sources on these cleaned protected area boundaries.

The Global Forest Watch (\texttt{GFW}) API, developed by the World Resources Institute, provides access to selected global forest monitoring datasets such as tree cover, loss, gain, biomass, and fire activity derived from satellite products like Hansen et al. [-@hansen_high-resolution_2013]. It allows users to delegate computations to a remote cloud infrastructure on these datasets and retrieve aggregated statistics for defined areas. \texttt{mapme.biodiversity} instead performs all processing locally or on user-managed servers and is designed to handle a wider set of environmental and socio-economic data sources.

The \texttt{MODIStsp} \texttt{R} package [@busetto_modistsp_2016] provides tools to download, mosaic, reproject, and subset a wide range of satellite products, including vegetation indices, land surface temperature, albedo, and burned areas—listed in its product catalog. It focuses exclusively on MODIS products.

The \texttt{sits} \texttt{R} package [@simoes_satellite_2021] prprocesses satellite imagery from Landsat, Sentinel, or MODIS. It builds time-series data cubes from these surface reflectance images and applies machine-learning models to classify land cover. Its inputs are direct measurements from Earth observation sensors, requiring modeling of reflectance dynamics over time. In contrast, \texttt{mapme.biodiversity}  relies on derived geospatial products that already summarize physical or biological processes (e.g., forest loss, fire occurrence, precipitation, population density) and uses them to compute area-based indicators over user-defined areas.

\texttt{BON in a Box}, developed by the Group on Earth Observations Biodiversity Observation Network (GEO BON), is an open platform for biodiversity monitoring and indicator computation [@griffith_boninabox_2025]. It enables users to assemble and share modular workflows that generate Essential Biodiversity Variables and policy-relevant indicators aligned with the Kunming–Montreal Global Biodiversity Framework. While \texttt{mapme.biodiversity} focuses on reproducible computation of area-based indicators within R, BON in a Box emphasizes cross-language interoperability and integration with national biodiversity monitoring systems. Griffith et al. (2025, Table S2, p. 18) explicitly identify \texttt{mapme.biodiversity} as an R package suitable for integration as a pipeline within BON in a Box.

# Functionality and Example Usage

Key features include:

- **Data acquisition and preparation**: automated download and preprocessing of global geospatial datasets with spatial–temporal filtering for user-defined areas of interest, optional local or cloud-based caching (e.g., through \texttt{vsis3/}).
- **Indicator computation and aggregation**: harmonized routines for summarizing and aggregating results across spatial units.
- **Scalability**: Utilizes existing R packages for spatial data handling (\texttt{terra}, \texttt{sf}), data manipulation (\texttt{dplyr}), and parallel processing (\texttt{future}) and progress monitoring (\texttt{progressr}) to handle large datasets. Supports multiple any vector area of interest as input, enabling the processing of many regions of interest in a single run.
- **Reproducibility**: supports standardized, modular, scriptable workflows to make analyses replication, auditing and sharing. 
- **Extensibility**: the framework allows users to add datasets add datasets and create their own indicators to meet specific research, monitoring or evaluation needs.
- **Interoperability**: outputs in standard geospatial formats, compatible with external GIS and statistical software.

The following minimal example illustrates a typical workflow:

```r
library(mapme.biodiversity)
# Define one or several areas of interest
aoi_path <- system.file("extdata", "gfw_sample.gpkg", 
package = "mapme.biodiversity")
aoi <- sf::read_sf(aoi_path)
# Get the resource data
res <- get_resources(aoi,
                     get_gfw_treecover(version = "GFC-2024-v1.12"),
                     get_gfw_lossyear(version = "GFC-2024-v1.12"))
# Compute the indicator
ind <- calc_indicators(res,
                       calc_treecover_area(years = 2000:2024, 
                       min_size = 1, min_cover = 30))
# Transform into a more suitable format
out <- portfolio_long(ind)
# plot the results
plot(out$datetime, out$value, col =  "blue", pch = 16, xlab = "year",
     ylab = sprintf("%s (%s)", out$variable[1], out$unit[1]), 
     main =  "Treecover loss")
```

# Availability

The \texttt{mapme.biodiversity} \texttt{R} package is implemented as an extension package to the \texttt{R} statistical computing environment [@rcore]. It is available on the Comprehensive R Archive Network (CRAN) [@mapme]. Development
versions are available on an online code repository (<https://github.com/mapme-initiative/mapme.biodiversity>). Documentation for the package can be found online (<https://mapme-initiative.github.io/mapme.biodiversity/>).

# Acknowledgments

The development and maintenance of the \texttt{mapme.biodiversity} package have been funded by the KfW German Development Bank. The authors thank all users and contributors who provided feedback and suggestions that helped improve the package.

# Conflicts of interest

This software was developed through collaboration between KfW staff, contracted developers, and independent researchers. Darius A. Görgen, Zivan Karaman, and Andreas Petutschnig contributed to the development under KfW funding; Johannes Schielein is employed by KfW; and Florent Bédécarrats uses the package for research activities, some of which are funded by the KfW. The KfW supports the aforementioned software development and research activities as a part of its mandate from the German State to support environment protection and sustainable development in less privileged countries. In this framework, the KfW sponsors the development of tools and studies to further enhance knowledge, accountability and evaluation on results of the policies it finances. The authors declare that KfW support enabled their work on the \texttt{mapme.biodiversity} package and did not hinder in any way the accuracy, reliability or performance of this software, nor the objectivity with which it is presented in the present article. 

# References
