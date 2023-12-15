devtools::load_all()
library(sf)
library(future)
library(progressr)

outdir <- file.path(tempdir(), "mapme")
dir.create(outdir)

aoi <- st_read("../portfolio_tc_donors.gpkg")[1, ]
aoi1 <- init_portfolio(aoi, years = 2010:2015, outdir = outdir)
aoi2 <- init_portfolio(aoi, years = 2010:2015, outdir = "dont-need-that")

aoi1 <- get_resources(aoi1, resources = "cci_lcc", download = TRUE)
aoi2 <- get_resources(aoi2, resources = "cci_lcc", download = FALSE)

timing1 <- system.time({aoi1 <- calc_indicators(aoi1, indicators = "cci_landcover")})
timing2 <- system.time({aoi2 <- calc_indicators(aoi2, indicators = "cci_landcover")})

timing1
timing2

identical(aoi1$cci_landcover, aoi2$cci_landcover)
