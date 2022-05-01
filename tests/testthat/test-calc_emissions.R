test_that("emissions works", {
  shp <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )
  treecover2000 <- list.files(system.file("res", "treecover2000",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)

  lossyear <- list.files(system.file("res", "lossyear",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)

  greenhouse <- list.files(system.file("res", "greenhouse",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)

  treecover2000 <- rast(treecover2000)
  lossyear <- rast(lossyear)
  greenhouse <- rast(greenhouse)

  attributes(shp)$years <- 1990:1999
  attributes(shp)$cores <- 1

  expect_warning(
    .calc_emissions(shp, treecover2000, lossyear, greenhouse),
    "Cannot calculate emissions statistics for years smaller than 2000"
  )

  attributes(shp)$years <- 2000:2005
  expect_equal(
    .calc_emissions(shp, treecover2000, lossyear, NULL),
    NA
  )

  expect_error(
    .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_size = "a"),
    "Argument 'min_size' for indicator 'emissions' must be a numeric value greater 0."
  )

  expect_error(
    .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_size = -10),
    "Argument 'min_size' for indicator 'emissions' must be a numeric value greater 0."
  )

  expect_error(
    .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_cover = "a"),
    "Argument 'min_cover' for indicator 'emissions' must be a numeric value between 0 and 100."
  )

  expect_error(
    .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_cover = -10),
    "Argument 'min_cover' for indicator 'emissions' must be a numeric value between 0 and 100."
  )

  expect_error(
    .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_cover = 110),
    "Argument 'min_cover' for indicator 'emissions' must be a numeric value between 0 and 100."
  )

  expect_snapshot(
    .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_size = 1, min_cover = 10)
  )

  attributes(shp)$years <- 1999:2005
  expect_warning(
    stat <- .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_size = 1, min_cover = 10),
    "Cannot calculate emissions statistics for years smaller than 2000."
  )
  expect_snapshot(stat)
  attributes(shp)$years <- 2000:2005
  stats_treecover <- .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_size = 1, min_cover = 10)
  stats_treeloss <- .calc_treeloss(shp, treecover2000, lossyear, greenhouse, min_size = 1, min_cover = 10)[, c(1, 2)]
  expect_equal(stats_treecover, stats_treecover)
})
