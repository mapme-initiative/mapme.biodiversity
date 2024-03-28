test_that("drought indicator works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nasa_grace <- list.files(system.file("res", "nasa_grace",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nasa_grace <- rast(nasa_grace)

  cdi <- calc_drought_indicator()
  result <- cdi(shp, nasa_grace)
  cdi <- calc_drought_indicator(stats = c("mean", "median", "sd"))
  result_multi_stats <- cdi(shp, nasa_grace)
  cdi <- calc_drought_indicator(engine = "zonal")
  result_zonal <- cdi(shp, nasa_grace)
  cdi <- calc_drought_indicator(engine = "extract")
  result_extract <- cdi(shp, nasa_grace)
  cdi <- calc_drought_indicator(engine = "exactextract")
  result_exact <- cdi(shp, nasa_grace)

  expect_equal(
    names(result[[1]]),
    c("wetness_mean", "date")
  )
  expect_equal(
    names(result_multi_stats[[1]]),
    c("wetness_mean", "wetness_median", "wetness_sd", "date")
  )
  expect_equal(
    names(result_zonal),
    names(result_extract)
  )
  expect_equal(
    names(result_zonal),
    names(result_exact)
  )
  expect_equal(
    result[[1]]$wetness_mean,
    result_zonal[[1]]$wetness_mean,
    tolerance = 1e-4
  )
  expect_equal(
    result_zonal[[1]]$wetness_mean,
    result_extract[[1]]$wetness_mean,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact[[1]]$wetness_mean
  )
})
