test_that("worldclim precipitation works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  precipitation <- list.files(system.file("res", "precipitation",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  precipitation <- rast(precipitation)
  attributes(shp)$years <- 2000:2022
  attributes(shp)$cores <- 1
  expect_error(
    .calc_wcprec(shp, precipitation, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_wcprec(shp, precipitation, stats_worldclim = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_wcprec(shp, precipitation)
  )
  expect_snapshot(
    .calc_wcprec(shp, precipitation, stats_worldclim = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_wcprec(shp, precipitation, engine = "extract")
  )
  expect_snapshot(
    .calc_wcprec(shp, precipitation, engine = "exactextract")
  )
  expect_snapshot(
    .calc_wcprec(shp, precipitation, engine = "zonal")
  )
})
