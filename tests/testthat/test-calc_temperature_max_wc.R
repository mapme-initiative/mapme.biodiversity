test_that("worldclim maximum temperature works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  worldclim_max_temperature <- list.files(system.file("res", "worldclim_max_temperature",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  worldclim_max_temperature <- rast(worldclim_max_temperature)

  cmx <- calc_temperature_max_wc()
  expect_true(is.null(cmx(x, NULL)))

  result <- cmx(x, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(stats = c("mean", "median", "sd"))
  result_multi_stat <- cmx(x, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(engine = "zonal")
  result_zonal <- cmx(x, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(engine = "extract")
  result_extract <- cmx(x, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(engine = "exactextract")
  result_exact <- cmx(x, worldclim_max_temperature)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  vars <- c("worldclim_tmax_mean", "worldclim_tmax_median", "worldclim_tmax_sd")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_equal(result_zonal$value, result_extract$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
