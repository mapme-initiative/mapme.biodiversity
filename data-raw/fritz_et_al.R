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
tif <- mapme.biodiversity:::.get_fritz_et_al(aoi, res_drivers = 100, rundir = tmp_loc)
drivers <- rast(tif)
drivers_crop <- crop(drivers, aoi)
dir.create("inst/res/fritz_et_al", showWarnings = FALSE)
writeRaster(drivers_crop, file.path("inst/res/fritz_et_al/", basename(tif)), overwrite = TRUE)
file.create(file.path("inst/res/fritz_et_al/", "Deforestation_Drivers_100m_IIASA.zip"))
