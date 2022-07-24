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
  attributes(shp)$cores <- 1
  expect_error(
    .calc_drought_indicator(shp, nasa_grace, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_drought_indicator(shp, nasa_grace, stats_drought = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasa_grace)
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasa_grace, engine = "zonal")
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasa_grace, stats = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasa_grace, engine = "extract")
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasa_grace, engine = "exactextract")
  )
})
