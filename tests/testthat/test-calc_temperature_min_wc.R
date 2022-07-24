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
  attributes(shp)$cores <- 1
  expect_error(
    .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_temperature_min_wc(shp, worldclim_min_temperature, stats_worldclim = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_temperature_min_wc(shp, worldclim_min_temperature)
  )
  expect_snapshot(
    .calc_temperature_min_wc(shp, worldclim_min_temperature, stats_worldclim = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "extract")
  )
  expect_snapshot(
    .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "exactextract")
  )
  expect_snapshot(
    .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "zonal")
  )
})
