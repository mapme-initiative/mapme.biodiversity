library(mapme.biodiversity)
library(sf)
library(terra)

shp <- read_sf(
  system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
              package = "mapme.biodiversity"
  )
)
outdir_base <- file.path("inst", "res")

for(gsw_statistic in mapme.biodiversity:::.gsw_statistics) {

  gsw_getter <- sprintf("get_global_surface_water_%s", gsw_statistic)
  res <- get_resources(shp, get(gsw_getter)())
  gsw_raster <- prep_resources(res)[[sprintf("global_surface_water_%s", gsw_statistic)]]

  gsw_raster_subset <- crop(gsw_raster, shp)
  gsw_raster_subset <- mask(gsw_raster_subset, shp)

  outdir_statistic <- file.path(outdir_base, paste0("global_surface_water_", gsw_statistic))
  dir.create(outdir_statistic, recursive = TRUE, showWarnings = FALSE)

  writeRaster(gsw_raster_subset,
              file.path(outdir_statistic, paste0(names(gsw_raster), ".tif")),
              gdal = c("COMPRESS=LZW", "PREDICTOR=2"), datatype = "INT1U",
              overwrite = TRUE)
}

