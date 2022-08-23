library(testthat)
library(mapme.biodiversity)
library(pbapply)
library(pillar)

pboptions(type = "none")
options(pillar.advice = FALSE)
test_check("mapme.biodiversity")
