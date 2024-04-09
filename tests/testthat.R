library(testthat)
library(mapme.biodiversity)

options(pillar.advice = FALSE)
mapme_options(verbose = FALSE)
test_check("mapme.biodiversity")
