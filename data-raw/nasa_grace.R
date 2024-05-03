## code to prepare `nasa_grace` dataset goes here
library(mapme.biodiversity)
library(terra)
library(sf)

tmp_loc <- tempfile()
dir.create(tmp_loc)
aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) |> st_read()

mapme_options(outdir = tmp_loc)

get_resources(aoi, get_nasa_grace(years = 2022))
tifs <- mapme.biodiversity:::.avail_resources("nasa_grace")[[1]]$location
nasa_grace <- rast(tifs)
nasa_grace_crop <- crop(nasa_grace, aoi)
dir.create("inst/res/nasa_grace/", showWarnings = FALSE)
writeRaster(nasa_grace_crop, file.path("inst/res/nasa_grace/", basename(tifs)),
  overwrite = TRUE, gdal = "COMPRESS=LZW", datatype = "INT1U"
)
