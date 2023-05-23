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
  attributes(shp)$years <- 2003:2022
  expect_error(
    .calc_drought_indicator(shp, nasa_grace, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_drought_indicator(shp, nasa_grace, stats_drought = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )

  result <- .calc_drought_indicator(shp, nasa_grace)
  result_multi_stats <- .calc_drought_indicator(shp, nasa_grace, stats = c("mean", "median", "sd"))
  result_zonal <- .calc_drought_indicator(shp, nasa_grace, engine = "zonal")
  result_extract <- .calc_drought_indicator(shp, nasa_grace, engine = "extract")
  result_exact <- .calc_drought_indicator(shp, nasa_grace, engine = "exactextract")

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
