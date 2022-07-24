test_that("terrain ruggedness index works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nasa_srtm <- list.files(system.file("res", "nasa_srtm",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nasa_srtm <- rast(nasa_srtm)
  attributes(shp)$years <- 2022
  attributes(shp)$cores <- 1
  expect_error(
    .calc_tri(shp, nasa_srtm, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_tri(shp, nasa_srtm, stats_tri = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_tri(shp, nasa_srtm)
  )
  expect_snapshot(
    .calc_tri(shp, nasa_srtm, stats = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_tri(shp, nasa_srtm, engine = "extract")
  )
  expect_snapshot(
    .calc_tri(shp, nasa_srtm, engine = "exactextract")
  )
})
