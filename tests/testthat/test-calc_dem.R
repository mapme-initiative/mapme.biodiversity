test_that("srtm dem works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  srtmdem <- list.files(system.file("res", "srtmdem",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  srtmdem <- rast(srtmdem)
  attributes(shp)$years <- 2022
  attributes(shp)$cores <- 1
  expect_error(
    .calc_dem(shp, srtmdem, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_dem(shp, srtmdem, stats_elevation = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_dem(shp, srtmdem)
  )
  expect_snapshot(
    .calc_dem(shp, srtmdem, stats = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_dem(shp, srtmdem, engine = "extract")
  )
  expect_snapshot(
    .calc_dem(shp, srtmdem, engine = "exactextract")
  )
})
