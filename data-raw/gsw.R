library(sf)
library(terra)

shp <- read_sf(
  system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
    package = "mapme.biodiversity"
  )
)
outdir_base <- file.path("inst", "res")

for(gsw_statistic in mapme.biodiversity:::.gsw_statistics) {
  gsw_raster_fname <- mapme.biodiversity:::.get_gsw(shp, statistic = gsw_statistic)
  gsw_raster <- rast(gsw_raster_fname)
  gsw_raster_subset <- crop(gsw_raster, shp)

  outdir_statistic <- file.path(outdir_base, paste0("gsw_", gsw_statistic))
  dir.create(outdir_statistic, recursive = TRUE, showWarnings = FALSE)

  writeRaster(gsw_raster_subset,
              file.path(outdir_statistic,basename(gsw_raster_fname)),
              overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "PREDICTOR=2"), datatype = "INT2U"
  )
}
