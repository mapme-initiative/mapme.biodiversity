## code to prepare `fritz_et_al` dataset goes here
library(mapme.biodiversity)
library(terra)
library(sf)

tmp_loc <- tempfile()
dir.create(tmp_loc)
x <- read_sf(
  system.file("extdata", "sierra_de_neiba_478140.gpkg",
    package = "mapme.biodiversity"
  )
)
mapme_options(outdir = tmp_loc, verbose = FALSE)
get_resources(x, get_fritz_et_al(resolution = 100))
drivers <- prep_resources(x)[["fritz_et_al"]]
dir.create("inst/res/fritz_et_al", showWarnings = FALSE)
writeRaster(drivers, "inst/res/fritz_et_al/geo_fritz_et_al_100m.tif",
  overwrite = TRUE
)
