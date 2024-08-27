library(sf)
library(terra)

x <- read_sf(system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
))
crs <- st_crs("ESRI:54009")
x <- st_transform(x, crs)
r <- rast(x, resolution = 30000, nlyrs = 2)
set.seed(43)
r[] <- round(runif(ncell(r) * nlyr(r), min = 1, max = 30))
names(r) <- c("Amphibians_SR_2023", "Birds_THR_SR_2023")
outdir <- "inst/res/iucn"
dir.create(outdir, showWarnings = FALSE)
writeRaster(r,
  filename = file.path(outdir, paste0(names(r), ".tif")),
  datatype = "INT1U", overwrite = TRUE
)
