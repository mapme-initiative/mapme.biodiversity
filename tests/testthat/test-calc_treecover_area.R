test_that("test gfw utils", {
  x <- read_sf(system.file("extdata", "gfw_sample.gpkg",
    package = "mapme.biodiversity"
  ))

  treecover <- rast(list.files(
    system.file("res", "gfw_treecover", package = "mapme.biodiversity"),
    pattern = ".tif$", full.names = TRUE
  ))

  lossyear <- rast(list.files(
    system.file("res", "gfw_lossyear", package = "mapme.biodiversity"),
    pattern = ".tif$", full.names = TRUE
  ))

  emissions <- rast(list.files(
    system.file("res", "gfw_emissions", package = "mapme.biodiversity"),
    pattern = ".tif$", full.names = TRUE
  ))

  expect_warning(y <- .gfw_check_years(1999:2000, "treecover"))
  expect_length(y, 1)
  expect_warning(y <- .gfw_check_years(1999, "treecover"))
  expect_length(y, 0)

  expect_false(.gfw_empty_raster(treecover))
  dummy <- treecover
  dummy[] <- NA
  expect_true(.gfw_empty_raster(dummy))
  dummy[] <- 0
  expect_true(.gfw_empty_raster(dummy))

  expect_error(.gfw_check_min_cover(1:2, "treecover"), "Argument 'min_cover'")
  expect_error(.gfw_check_min_cover("a", "treecover"), "Argument 'min_cover'")
  expect_error(.gfw_check_min_cover(-1, "treecover"), "Argument 'min_cover'")
  expect_error(.gfw_check_min_cover(101, "treecover"), "Argument 'min_cover'")
  expect_silent(y <- .gfw_check_min_cover(50, "treecover"))

  expect_error(.gfw_check_min_size(1:2, "treecover"), "Argument 'min_size'")
  expect_error(.gfw_check_min_size("a", "treecover"), "Argument 'min_size'")
  expect_error(.gfw_check_min_size(-1, "treecover"), "Argument 'min_size'")
  expect_silent(.gfw_check_min_size(5, "treecover"))

  expect_true(inherits(.gfw_calc_patches(treecover), "SpatRaster"))

  lossyear <- treecover
  lossyear[] <- round(runif(ncell(lossyear), 0, 20))
  emissions <- emissions
  emissions[] <- round(runif(ncell(lossyear), 0, 100))

  y <- .gfw_prep_rasters(x, treecover, lossyear, cover = 30)
  expect_true(inherits(y, "SpatRaster"))
  expect_equal(names(y), c("treecover", "lossyear", "patches"))
  y <- .gfw_prep_rasters(x, treecover, lossyear, emissions, 30)
  expect_true(inherits(y, "SpatRaster"))
  expect_equal(names(y), c("treecover", "lossyear", "patches", "emissions"))
  expect_error(.gfw_prep_rasters(x, treecover, lossyear, 30))


  df <- exactextractr::exact_extract(y, x, coverage_area = TRUE)[[1]]
  expect_silent(df <- .prep_gfw_data(df, 5))
  expect_equal(names(df), c(
    "treecover", "lossyear",
    "patches", "emissions", "coverage_area"
  ))

  expect_silent(area <- .sum_gfw(df, "coverage_area"))
  expect_equal(names(area), c("years", "coverage_area"))
  expect_silent(emis <- .sum_gfw(df, "emissions"))
  expect_equal(names(emis), c("years", "emissions"))
})

test_that("treecover works", {
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

  attributes(x)$years <- 2000:2005

  expect_equal(.calc_treecover_area(x, gfw_treecover, NULL), NA)
  result <- .calc_treecover_area(x, gfw_treecover, gfw_lossyear,
    min_size = 1, min_cover = 10
  )
  expect_equal(names(result), c("years", "treecover"))
  expect_equal(result$years, c(2000:2005))
  expect_snapshot(result$treecover)

  stats_treeloss <- .calc_treecover_area_and_emissions(
    x, gfw_treecover,
    gfw_lossyear, gfw_emissions,
    min_size = 1, min_cover = 10
  )[, c(1, 3)]
  expect_equal(result$treecover, stats_treeloss$treecover)
})
