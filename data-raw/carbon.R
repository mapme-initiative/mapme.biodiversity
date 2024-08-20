library(mapme.biodiversity)
library(terra)
library(sf)

x <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
  package = "mapme.biodiversity"
) %>%
  read_sf()

outdir <- tempfile()
dir.create(outdir)
mapme_options(outdir = outdir)

options(timeout = 600)
get_resources(x, get_irr_carbon(), get_vul_carbon(), get_man_carbon())
resources <- prep_resources(x)
tindex <- mapme.biodiversity:::.avail_resources()

outdir <- "inst/resources/irr_carbon"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
writeRaster(resources$irr_carbon,
  file.path(outdir, tindex$irr_carbon$filename),
  datatype = "INT2U", overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW"))
)

outdir <- "inst/resources/vul_carbon"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
writeRaster(resources$vul_carbon,
  file.path(outdir, tindex$vul_carbon$filename),
  datatype = "INT2U", overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW"))
)

outdir <- "inst/resources/man_carbon"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
writeRaster(resources$man_carbon,
  file.path(outdir, tindex$man_carbon$filename),
  datatype = "INT2U", overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW"))
)
