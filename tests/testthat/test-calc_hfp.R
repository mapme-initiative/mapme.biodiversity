test_that("calc_hfp works", {
  x <- read_sf(system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
    package = "mapme.biodiversity"
  ))
  hfp <- rast(system.file("res", "humanfootprint", "hfp2010.tif",
    package = "mapme.biodiversity"
  ))
  hf <- calc_humanfootprint(stats = c("mean", "median"))
  expect_silent(result <- hf(x, hfp))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, c("humanfootprint_mean", "humanfootprint_median"))
  expect_equal(result$value, c(2.87, 2.62), tolerance = 0.01)
})
