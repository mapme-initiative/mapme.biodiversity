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

To prevent biodiversity losses at scale, conservation researchers require area-based
indicators that monitor the state of portfolios of intervention areas over time.
However, relevant spatial data resources are scattered across data repositories
and servers for which access patterns can differ significantly. Most other
software usually focuses on the analysis of a single (group of) data resource,
increasing the cognitive burden on researchers who have to learn multiple interfaces
and put substantial efforts into harmonizing output structures.

The \texttt{mapme.biodiversity} \texttt{R} package provides a single access pattern
to a diverse set of conservation related data resources from diverse sources.
It provides a common interface to derive area-based indicators for conservation
portfolios in a standardized output format. Further workflows and analysis of the
indicators can be conducted in the \texttt{R} computing environment or with
other tools of one's liking through the serialization of data to standard spatial
formats. It thus helps individuals and groups active in conservation research
to streamline their spatial data acqusition process.

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
