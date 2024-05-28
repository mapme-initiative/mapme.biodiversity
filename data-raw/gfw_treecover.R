library(sf)
library(terra)
library(mapme.biodiversity)

tmp_loc <- tempfile()
dir.create(tmp_loc)
mapme_options(outdir = tmp_loc)

x <- system.file("extdata", "gfw_sample.gpkg",
  package = "mapme.biodiversity"
)
x <- st_read(x)

get_resources(
  x,
  get_gfw_treecover(version = "GFC-2023-v1.11"),
  get_gfw_lossyear(version = "GFC-2023-v1.11"),
  get_gfw_emissions()
)

rs <- prep_resources(x)

unlink("inst/res/gfw_treecover", force = T, recursive = TRUE)
dir.create("inst/res/gfw_treecover/")
writeRaster(rs$gfw_treecover,
  filename = "inst/res/gfw_treecover/Hansen_GFC-2023-v1.11_treecover2000_20N_080W.tif",
  gdal = "COMPRESS=LZW", datatype = "INT1U"
)


unlink("inst/res/gfw_lossyear", force = T, recursive = TRUE)
dir.create("inst/res/gfw_lossyear/")
writeRaster(rs$gfw_lossyear,
  filename = "inst/res/gfw_lossyear/Hansen_GFC-2023-v1.11_lossyear_20N_080W.tif",
  gdal = "COMPRESS=LZW", datatype = "INT1U"
)

unlink("inst/res/gfw_emissions", force = T, recursive = TRUE)
dir.create("inst/res/gfw_emissions/")
writeRaster(rs$gfw_emissions,
  filename = "inst/res/gfw_emissions/gfw_forest_carbon_gross_emissions_Mg_CO2e_px_20N_080W.tif",
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2"), datatype = "FLT4S"
)
