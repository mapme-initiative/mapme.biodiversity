devtools::load_all()
library(sf)
library(future)
library(progressr)

aoi <- st_read("../portfolio_tc_donors.gpkg")[1, ]
aoi <- init_portfolio(aoi, years = 2010, outdir = "dont-need-that")
aoi <- get_resources(aoi, resources = "buildings", download = FALSE)

attributes(aoi)$resources
timing <- system.time({aoi <- calc_indicators(aoi, indicators = "building_count")})
timing
aoi$building_count
