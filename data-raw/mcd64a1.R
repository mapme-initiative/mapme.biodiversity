library(mapme.biodiversity)
library(terra)
library(sf)

x <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
)

x <- st_read(x)
mapme_options(outdir = NULL)
get_resources(x, get_mcd64a1(2010))
mcd64a1 <- prep_resources(x)$mcd64a1

dir.create("inst/res/mcd64a1/", showWarnings = FALSE)
mcd64a1 <- aggregate(mcd64a1, 4)
writeRaster(
  mcd64a1,
  file.path("inst/res/mcd64a1/", paste0(names(mcd64a1), ".tif")),
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE"),
  datatype = "INT1U"
)
