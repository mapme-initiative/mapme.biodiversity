test_that("worldclim minimum temperature works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  mintemperature <- list.files(system.file("res", "mintemperature",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  mintemperature <- rast(mintemperature)
  attributes(shp)$years <- 2000:2022
  attributes(shp)$cores <- 1
  expect_error(
    .calc_wctmin(shp, mintemperature, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_wctmin(shp, mintemperature, stats_worldclim = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_wctmin(shp, mintemperature)
  )
  expect_snapshot(
    .calc_wctmin(shp, mintemperature, stats_worldclim = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_wctmin(shp, mintemperature, engine = "extract")
  )
  expect_snapshot(
    .calc_wctmin(shp, mintemperature, engine = "exactextract")
  )
  expect_snapshot(
    .calc_wctmin(shp, mintemperature, engine = "zonal")
  )
})
