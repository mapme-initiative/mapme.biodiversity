test_that("drought indicator works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nasagrace <- list.files(system.file("res", "nasagrace",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nasagrace <- rast(nasagrace)
  attributes(shp)$years <- 2003:2022
  attributes(shp)$cores <- 1
  expect_error(
    .calc_drought_indicator(shp, nasagrace, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_drought_indicator(shp, nasagrace, stats_drought = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasagrace)
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasagrace, engine = "zonal")
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasagrace, stats = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasagrace, engine = "extract")
  )
  expect_snapshot(
    .calc_drought_indicator(shp, nasagrace, engine = "exactextract")
  )
})
