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

get_resources(x, get_humanfootprint(years = 2010))
hfp <- prep_resources(x)$humanfootprint
writeRaster(hfp,
  filename = file.path("inst/resources/humanfootprint/", paste0(names(hfp), ".tif")),
  datatype = "FLT4S", gdal = c("COMPRESS=LZW"),
  overwrite = TRUE
)
