library(sf)
library(terra)
library(mapme.biodiversity)

# Load the spatial object (x) for cropping
x <- read_sf(system.file("extdata", "sierra_de_neiba_478140.gpkg", package = "mapme.biodiversity"))

# Set output directory for cropped data
outdir <- "inst/resources/accessibility_2000"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Set the temporary directory for processing
tmp_loc <- tempfile()
dir.create(tmp_loc)
mapme_options(outdir = tmp_loc)

# Download and process the full accessibility data
get_resources(x, get_accessibility_2000())
resources <- prep_resources(x)

# Crop the accessibility data to the extent of x
access_raster <- resources$accessibility_2000

# Save the cropped raster in the package inst/resources directory
writeRaster(access_raster,
            filename = file.path(outdir, "accessibility_2000_cropped.tif"),
            datatype = "INT4S",
            overwrite = TRUE,
            wopt = list(gdal = c("COMPRESS=LZW"))
)
