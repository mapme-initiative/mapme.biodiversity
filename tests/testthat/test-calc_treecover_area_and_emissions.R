test_that("treecover area and emissions works", {
  shp <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )

  gfw_treecover <- list.files(system.file("res", "gfw_treecover",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)

  gfw_lossyear <- list.files(system.file("res", "gfw_lossyear",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)

  gfw_emissions <- list.files(system.file("res", "gfw_emissions",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)

  gfw_treecover <- rast(gfw_treecover)
  gfw_lossyear <- rast(gfw_lossyear)
  gfw_emissions <- rast(gfw_emissions)

  attributes(shp)$years <- 1990:1999
  attributes(shp)$cores <- 1

  expect_warning(
    .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions),
    "Cannot calculate treeloss statistics for years smaller than 2000"
  )

  attributes(shp)$years <- 2000:2005
  expect_equal(
    .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, NULL),
    NA
  )

  expect_error(
    .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_size = "a"),
    "Argument 'min_size' for indicator 'treeloss' must be a numeric value greater 0."
  )

  expect_error(
    .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_size = -10),
    "Argument 'min_size' for indicator 'treeloss' must be a numeric value greater 0."
  )

  expect_error(
    .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_cover = "a"),
    "Argument 'min_cover' for indicator 'treeloss' must be a numeric value between 0 and 100."
  )

  expect_error(
    .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_cover = -10),
    "Argument 'min_cover' for indicator 'treeloss' must be a numeric value between 0 and 100."
  )

  expect_error(
    .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_cover = 110),
    "Argument 'min_cover' for indicator 'treeloss' must be a numeric value between 0 and 100."
  )

  expect_snapshot(
    .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_size = 1, min_cover = 10)
  )

  attributes(shp)$years <- 1999:2005
  expect_warning(
    stat <- .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_size = 1, min_cover = 10),
    "Cannot calculate treeloss statistics for years smaller than 2000."
  )

  expect_snapshot(stat)
  attributes(shp)$years <- 2000:2005
  stats_treeloss <- .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_size = 1, min_cover = 10)[, c(1, 2)]
  stats_emissions <- .calc_treecoverloss_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions, min_size = 1, min_cover = 10)
  expect_equal(stats_treeloss, stats_emissions)
})
