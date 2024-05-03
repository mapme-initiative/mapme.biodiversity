## code to prepare `fritz_et_al` dataset goes here
library(mapme.biodiversity)
library(terra)
library(sf)

tmp_loc <- tempfile()
outdir <- "inst/res/chirps/"

dir.create(tmp_loc)
aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) %>% st_read()

gch <- get_chirps(years = 2010)
tifs <- gch(aoi, outdir = tmp_loc)
chirps <- lapply(tifs, function(x) rast(x) %>% crop(aoi)) %>% rast()
archives <- list.files(tmp_loc, pattern = "*.gz$")

dir.create(outdir, showWarnings = FALSE)
writeRaster(chirps, file.path(outdir, basename(tifs)),
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW", "PREDICTOR=2"), datatype = "INT2U"
)
file.create(file.path(outdir, archives))
