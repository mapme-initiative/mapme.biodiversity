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
tif <- mapme.biodiversity:::.get_nasa_srtm(aoi, download_srtm = TRUE, rundir = tmp_loc)
elevation <- rast(tif)
elevation_crop <- crop(elevation, aoi)
dir.create("inst/res/nasa_srtm/", showWarnings = FALSE)
elevation_crop <- aggregate(elevation_crop, 4)
writeRaster(elevation_crop, file.path("inst/res/nasa_srtm/", basename(tif)),
  overwrite = TRUE, gdal = c("COMPRESS=LZW", "PREDICTOR=2"), datatype = "INT2U"
)
