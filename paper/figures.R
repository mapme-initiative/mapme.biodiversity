library(mapme.biodiversity)
# Define one or several areas of interest
aoi_path <- system.file(
    "extdata",
    "gfw_sample.gpkg",
    package = "mapme.biodiversity"
)
aoi <- sf::read_sf(aoi_path)
# use remote resources
mapme_options(outdir = NULL)
# Get the resource data
res <- get_resources(
    aoi,
    get_gfw_treecover(version = "GFC-2024-v1.12"),
    get_gfw_lossyear(version = "GFC-2024-v1.12")
)
# Compute the indicator
ind <- calc_indicators(
    res,
    calc_treecover_area(years = 2000:2024, min_size = 1, min_cover = 30)
)
# Transform into long format for plotting
out <- portfolio_long(ind)

# plot the results
svg("paper/Figure1.svg", width = 11, height = 9)
plot(
    out$datetime,
    out$value,
    col = "blue",
    pch = 16,
    xlab = "year",
    ylab = sprintf("%s (%s)", out$variable[1], out$unit[1]),
    main = "Treecover loss"
)
dev.off()
