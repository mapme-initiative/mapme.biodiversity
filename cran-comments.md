## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release. mapme.biodiversity was designed to aid the statistical
  analysis of various spatial data sets (raster and vector formats) for potentially
  large portfolios. It abstracts away the overhead usually associated with
  spatio-temporal matching of geographical data sets, allowing users to focus
  on their analysis. The package currently allows users to download 12 different
  spatial data sets and the calculation of 17 different indicators. The framework
  is highly extensible and we wish to attract a user-base that eventually will 
  take part in extending the functionality. Internally, it is based on the well
  established [sf](https://cran.r-project.org/web/packages/sf/index.html) package 
  for vector operations and the [terra](https://cran.r-project.org/web/packages/terra/index.html) 
  package for raster operations. 
  
* We test the package against the current R release on MacOS, Windows and Ubuntu,
  and against the development and old-release of R on Ubuntu using GitHub Actions.

* R CMD check on the aforementioned configurations produced no warnings nor notes.
