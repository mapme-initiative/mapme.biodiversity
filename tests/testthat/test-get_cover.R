library(sf)
library(dplyr)
aoi = st_read(system.file("extdata", "aoi_polys.gpkg", package="mapme.biodiversity"))

test_that("get_cover", {
  expect_snapshot_output(
    aoi %>%
      init_portfolio(years = 2004:2008, outdir = system.file("extdata", package="mapme.biodiversity"), tmpdir = tempdir(), cores = 2) %>%
      get_resources(c("treecover", "lossyear")) %>%
      calc_indicator("cover", minSize = 10, minCover = 50) %>%
      select(cover) %>%
      unnest(cover)
  )
})
