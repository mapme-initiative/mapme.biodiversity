library(mapme.biodiversity)
library(future)
library(terra)
library(sf)

tmp_loc <- tempfile()
dir.create(tmp_loc)
mapme_options(outdir = tmp_loc)

x <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) %>% st_read()

plan(multisession, workers = 6)
get_resources(
  x, get_worldclim_max_temperature(years = 2018, resolution = "2.5m"),
  get_worldclim_min_temperature(years = 2018, resolution = "2.5m"),
  get_worldclim_precipitation(years = 2018, resolution = "2.5m")
)
plan(sequential)

resources <- prep_resources(x)

wctmax <- mask(resources$worldclim_max_temperature, x)
outdir <- "inst/res/worldclim_max_temperature/"
dir.create(outdir, showWarnings = FALSE)
writeRaster(wctmax, file.path(outdir, paste0(names(wctmax), ".tif")),
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW", "PREDICTOR=2")
)

wctmin <- mask(resources$worldclim_min_temperature, x)
outdir <- "inst/res/worldclim_min_temperature/"
dir.create(outdir, showWarnings = FALSE)
writeRaster(wctmin, file.path(outdir, paste0(names(wctmin), ".tif")),
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW", "PREDICTOR=2")
)

wcprec <- mask(resources$worldclim_precipitation, x)
outdir <- "inst/res/worldclim_precipitation/"
dir.create(outdir, showWarnings = FALSE)
writeRaster(wcprec, file.path(outdir, paste0(names(wcprec), ".tif")),
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW", "PREDICTOR=2")
)
