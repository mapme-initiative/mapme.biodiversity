# test-calc_worldpop.R

library(sf)
library(terra)
library(testthat)

test_that("population count works", {
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
    .calc_worldpop(shp, worldpop, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_worldpop(shp, worldpop, stats_worldpop = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_worldpop(shp, worldpop)
  )
  expect_snapshot(
    .calc_worldpop(shp, worldpop, stats_worldpop = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_worldpop(shp, worldpop, engine = "extract")
  )
  expect_snapshot(
    .calc_worldpop(shp, worldpop, engine = "exactextract")
  )
  expect_snapshot(
    .calc_worldpop(shp, worldpop, engine = "zonal")
  )
})
