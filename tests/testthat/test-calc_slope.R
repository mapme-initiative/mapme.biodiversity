test_that("slope calculation works", {
  # Load the polygon data for testing
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
                package = "mapme.biodiversity"
    )
  )

  # Load the NASA SRTM data required for slope calculation
  nasa_srtm <- list.files(system.file("res", "nasa_srtm",
                                      package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nasa_srtm <- rast(nasa_srtm)

  # Test calc_slope function with default settings
  cslope <- calc_slope()
  expect_true(is.null(cslope(x, NULL)))
  result <- cslope(x, nasa_srtm)

  # Test calc_slope function with multiple statistics
  cslope <- calc_slope(stats = c("mean", "median", "sd", "var"))
  result_multi_stat <- cslope(x, nasa_srtm)

  # Test calc_slope function with "zonal" engine
  cslope <- calc_slope(engine = "zonal")
  result_zonal <- cslope(x, nasa_srtm)

  # Test calc_slope function with "extract" engine
  cslope <- calc_slope(engine = "extract")
  result_extract <- cslope(x, nasa_srtm)

  # Test calc_slope function with "exactextract" engine
  cslope <- calc_slope(engine = "exactextract")
  result_exact <- cslope(x, nasa_srtm)

  # Ensure no errors and correct structure with various engines and settings
  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  # Check that the correct variables are returned for multiple statistics
  vars <- c("slope_mean", "slope_median", "slope_sd", "slope_var")
  expect_equal(unique(result_multi_stat$variable), vars)

  # Check that zonal and extract engines produce similar results
  expect_equal(result_zonal$value, result_extract$value, tolerance = 1e-4)

  # Capture a snapshot of the exact extract values for future comparison
  expect_snapshot(result_exact$value)
})
