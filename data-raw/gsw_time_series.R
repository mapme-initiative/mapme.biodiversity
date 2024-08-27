library(dplyr)
library(mapme.biodiversity)
library(sf)
library(terra)

years <- 2000:2001

aoi <- read_sf(
  system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
              package = "mapme.biodiversity"
  )
)
x <- get_resources(aoi, get_gsw_time_series(years = years))

gsw_time_series <- prep_resources(x)
gsw_time_series <- gsw_time_series [[1]]

outdir <- "inst/res/gsw_time_series"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

gsw_time_series <- mask(gsw_time_series, x)

for(lyr_id in seq_len(nlyr(gsw_time_series))) {
  lyr_name <- names(gsw_time_series) [lyr_id]
  lyr_name <- sub("VER5-0_yearlyClassification", "v5_", lyr_name)
  fname <- paste0(file.path(outdir, lyr_name), ".tif")
  lyr_subset <- subset(gsw_time_series, lyr_id)
  writeRaster(lyr_subset, fname,
              gdal = c("COMPRESS=LZW", "PREDICTOR=2"), datatype = "INT1U",
              overwrite = TRUE)
}
