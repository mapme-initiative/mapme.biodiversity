test_that("treecover area and emissions works", {
  x <- read_sf(system.file("extdata", "gfw_sample.gpkg",
    package = "mapme.biodiversity"
  ))

  gfw_treecover <- rast(list.files(
    system.file("res", "gfw_treecover", package = "mapme.biodiversity"),
    pattern = ".tif$", full.names = TRUE
  ))

  gfw_lossyear <- rast(list.files(
    system.file("res", "gfw_lossyear", package = "mapme.biodiversity"),
    pattern = ".tif$", full.names = TRUE
  ))

  gfw_emissions <- rast(list.files(
    system.file("res", "gfw_emissions", package = "mapme.biodiversity"),
    pattern = ".tif$", full.names = TRUE
  ))

  years <- 2000:2005
  min_size <- 1
  min_cover <- 10
  tae <- calc_treecover_area_and_emissions(years, min_size, min_cover)
  expect_equal(tae(x, gfw_treecover, gfw_lossyear, NULL), NA)

  result <- tae(x, gfw_treecover, gfw_lossyear, gfw_emissions)

  expect_equal(names(result), c("years", "emissions", "treecover"))
  expect_equal(result$years, c(2000:2005))
  expect_snapshot(result$treecover)
  expect_snapshot(result$emissions)

  te <- calc_treecoverloss_emissions(years, min_size, min_cover)
  stats_emissions <- te(x, gfw_treecover, gfw_lossyear, gfw_emissions)
  expect_equal(result$emissions, stats_emissions$emissions, tolerance = 1e-4)

  # test that emissions and forest loss are returned as 0 if no loss occurs
  gfw_lossyear[gfw_lossyear == 3] <- 1
  stats_treeloss <- tae(x, gfw_treecover, gfw_lossyear, gfw_emissions)
  expect_equal(stats_treeloss$emissions[stats_treeloss$years == 2003], 0)
})
