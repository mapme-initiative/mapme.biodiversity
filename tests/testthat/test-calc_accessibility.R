test_that("accessibility works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  traveltime <- list.files(system.file("res", "traveltime",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  traveltime <- rast(traveltime)
  attributes(shp)$years <- 2015
  attributes(shp)$cores <- 1
  expect_error(
    .calc_accessibility(shp, traveltime, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_accessibility(shp, traveltime, stats_accessibility = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_accessibility(shp, traveltime)
  )
  expect_snapshot(
    .calc_accessibility(shp, traveltime, engine = "zonal")
  )
  expect_snapshot(
    .calc_accessibility(shp, traveltime, stats = c("mean", "median", "sd", "min", "max", "sum", "var"))
  )
  expect_snapshot(
    .calc_accessibility(shp, traveltime, engine = "extract")
  )
  expect_snapshot(
    .calc_accessibility(shp, traveltime, engine = "exactextract")
  )
})
