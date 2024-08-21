library(mapme.biodiversity)
library(terra)
library(sf)

aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
  package = "mapme.biodiversity"
) %>%
  read_sf()

outdir <- tempfile()
dir.create(outdir)
mapme_options(outdir = outdir)

get_resources(x, get_ipbes_biomes())
biome <- prep_resources(x)$ipbes_biomes

writeRaster(biome,
  filename = file.path("inst/resources/ipbes_biomes/", paste0(names(biome), ".tif")),
  datatype = "INT1U", gdal = c("COMPRESS=LZW"),
  overwrite = TRUE
)
