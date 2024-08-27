library(sf)

aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
            package = "mapme.biodiversity")
aoi <- read_sf(aoi)

sf_use_s2(FALSE)
aoi <- suppressWarnings(
  st_simplify(st_buffer(aoi, dist = -0.01), dTolerance = 0.001)
)
sf_use_s2(TRUE)
aoi [, 1:4] <- NULL

outdir <- "inst/res/key_biodiversity_areas"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
write_sf(aoi, file.path(outdir, "kbas.gpkg"))
