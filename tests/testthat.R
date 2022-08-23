library(testthat)
library(mapme.biodiversity)
library(pbapply)

pboptions(type = "none")
options(pillar.advice = TRUE)
test_check("mapme.biodiversity")
