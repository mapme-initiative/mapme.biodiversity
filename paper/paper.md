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
affiliations:
 - name: Department of Landscape Ecology, University of Münster, Germany
   index: 1
date: "05 Juni 2025"
bibliography: paper.bib
output: rticles::joss_article
journal: JOSS
link-citations: yes
year: 2025
software_repository_url: www.github.com/mapme-initiative/mapme.biodiversity
submitted: unsubmitted
published: unpublished
---

# Summary

The \texttt{mapme.biodiversity} \texttt{R} package provides access to data and
analyisis routines to several datasets relevant to conservation research. It can
be used by scientists and practitioners to analyse conservation portfolios in a
consistent way. The software integrates well into existing workflows and spatial
analyses using the \texttt{R} programming environment. It has been used in impact
evaluations to assess the effectivness of international donor interventions to
reduce forest cover loss.

# Statement of need

To prevent biodiversity loss at scale, conservation researchers and practitioners require area-based indicators derived from diverse spatial datasets. Monitoring efforts often involve sets of intervention areas that must be tracked consistently over time. However, relevant data sources are dispersed across multiple repositories and platforms, each with distinct access protocols, formats, and documentation standards.

Most existing tools focus on a specific data source or domain, offering limited interoperability and requiring users to learn multiple interfaces. This fragmentation imposes a high cognitive and technical burden, especially on users who are not remote sensing specialists but need spatial data for research, monitoring, or policy evaluation.

The \texttt{mapme.biodiversity} \texttt{R} package provides a unified interface to access and process a wide range of spatial datasets relevant to conservation and environmental management. It enables users to derive standardized, area-based indicators at scale, supports reproducible workflows in \texttt{R}, and facilitates integration with other tools via export to standard spatial formats.

Originally developed for conservation monitoring, the package has also found applications in adjacent domains such as agriculture, rural development, food security, and infrastructure planning. It offers a transparent and auditable alternative to proprietary platform-as-a-service (PaaS) solutions like Google Earth Engine or Microsoft Planetary Computer, which may be inaccessible to users in low-connectivity settings, non-academic institutions, or those requiring long-term reproducibility not guaranteed by commercial platforms.

# Applications

- KfW: Impact evaluations (Melvin)
- KfW: Reporting to Ministry (Johannes/Sven)
- IRD: Research projects & capacity development (Florent)
- WWF and others?

# Comparison with other software packages

- JRC's DOPA: https://www.preprints.org/manuscript/202408.1146/v1 & https://github.com/giacomo-gcad/dopa_workflow/
- wdpar: https://github.com/prioritizr/wdpar
- Red List Indices: https://github.com/red-list-ecosystem/rle_indices
- GFW API: https://data-api.globalforestwatch.org/
- Others?

# Availability

The \texttt{mapme.biodiversity} \texttt{R} package is implemented as an extension
package to the \texttt{R} statistical computing environment [@rcore]. It is
available on the Comprehensive R Archive Network (CRAN) [@mapme]. Development
versions are available on an online code repository (<https://github.com/mapme-initiative/mapme.biodiversity>).
Documentation for the package can be found online (<https://mapme-initiative.github.io/mapme.biodiversity/>).

# Acknowledgments

Author Darius A. Görgen was funded by the generous support of the Kreditanstalt für Wiederaufbau (KfW).

# Conflict of interest

The authors declares no conflict of interest.

# References
