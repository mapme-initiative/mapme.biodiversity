test_that("worldclim maximum temperature works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  maxtemperature <- list.files(system.file("res", "maxtemperature",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  maxtemperature <- rast(maxtemperature)
  attributes(shp)$years <- 2000:2022
  attributes(shp)$cores <- 1
  expect_error(
    .calc_wctmax(shp, maxtemperature, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_wctmax(shp, maxtemperature, stats_worldclim = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_wctmax(shp, maxtemperature)
  )
  expect_snapshot(
    .calc_wctmax(shp, maxtemperature, stats_worldclim = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_wctmax(shp, maxtemperature, engine = "extract")
  )
  expect_snapshot(
    .calc_wctmax(shp, maxtemperature, engine = "exactextract")
  )
  expect_snapshot(
    .calc_wctmax(shp, maxtemperature, engine = "zonal")
  )
})
