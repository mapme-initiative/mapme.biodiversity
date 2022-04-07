test_that(".calc_popcount works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  worldpop <- list.files(system.file("res", "worldpop",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  worldpop <- rast(worldpop)
  attributes(shp)$years <- 2000:2022
  attributes(shp)$cores <- 1
  expect_error(
    .calc_popcount(shp, worldpop, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_popcount(shp, worldpop, stats_popcount = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_popcount(shp, worldpop)
  )
  expect_snapshot(
    .calc_popcount(shp, worldpop, stats_popcount = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_popcount(shp, worldpop, engine = "extract")
  )
  expect_snapshot(
    .calc_popcount(shp, worldpop, engine = "exactextract")
  )
  expect_snapshot(
    .calc_popcount(shp, worldpop, engine = "zonal")
  )
})
