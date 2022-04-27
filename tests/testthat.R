library(testthat)
library(mapme.biodiversity)
library(pbapply)

pboptions(type = "none")

temp_loc <- file.path(tempdir(), "mapme.biodiversity")
dir.create(temp_loc)
resource_dir <- system.file("res", package = "mapme.biodiversity")
file.copy(resource_dir, temp_loc, recursive = TRUE)

test_check("mapme.biodiversity")

unlink(temp_loc, recursive = TRUE, force = TRUE)
