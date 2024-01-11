test_that("worldclim minimum temperature works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  worldclim_min_temperature <- list.files(system.file("res", "worldclim_min_temperature",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  worldclim_min_temperature <- rast(worldclim_min_temperature)
  attributes(shp)$years <- 2000:2022
  expect_error(
    .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_temperature_min_wc(shp, worldclim_min_temperature, stats_worldclim = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )

  result <- .calc_temperature_min_wc(shp, worldclim_min_temperature)
  result_multi_stat <- .calc_temperature_min_wc(shp, worldclim_min_temperature, stats_worldclim = c("mean", "median", "sd"))
  result_zonal <- .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "zonal")
  result_extract <- .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "extract")
  result_exact <- .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "exactextract")

  expect_equal(
    names(result),
    c("tmin_mean", "date")
  )
  expect_equal(
    names(result_multi_stat),
    c("tmin_mean", "tmin_median", "tmin_sd", "date")
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
    result_zonal$tmin_mean,
    result_extract$tmin_mean,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact$tmin_mean
  )
})
