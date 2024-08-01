library(mapme.biodiversity)
library(terra)
library(sf)

tmp_loc <- tempfile()
outdir <- "inst/res/chelsa/"

dir.create(tmp_loc)
x <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) %>% st_read()

library(future)
plan(multisession, workers = 6)
get_resources(x, get_chelsa(2010))
plan(sequential)
chelsa <- prep_resources(x)$chelsa
chelsa <- mask(chelsa, x)

dir.create(outdir, showWarnings = FALSE)
writeRaster(chelsa, file.path(outdir, paste0(names(chelsa), ".tif")),
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW", "PREDICTOR=2"), datatype = "INT2U"
)
