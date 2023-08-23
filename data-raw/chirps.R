## code to prepare `fritz_et_al` dataset goes here
library(mapme.biodiversity)
library(terra)
library(sf)

tmp_loc <- tempfile()
dir.create(tmp_loc)
aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) |> st_read()

attr(aoi, "testing") <- FALSE
tifs <- mapme.biodiversity:::.get_chirps(aoi, rundir = tmp_loc, verbose = T)
chirps <- rast(tifs)
archives <- list.files(tmp_loc, pattern = "*.gz$")

chirps_crop <- crop(chirps, aoi)
outdir <- "inst/res/chirps/"
dir.create(outdir, showWarnings = FALSE)
# chirps_crop <- aggregate(chirps_crop, 4)
writeRaster(chirps_crop, file.path(outdir, basename(tifs)),
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW", "PREDICTOR=2"), datatype = "INT2U"
)
file.create(file.path(outdir, archives))
