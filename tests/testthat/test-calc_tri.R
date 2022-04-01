# test-calc_tri.R

library(sf)
library(terra)
library(testthat)

test_that("terrain ruggedness index works", {
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
    .calc_tri(shp, srtmdem, engine = "not-available"),
    "Engine not-available is not an available engine. Please choose one of: zonal, extract, exactextract"
  )
  expect_error(
    .calc_tri(shp, srtmdem, stats_tri = "not-available"),
    "Stat not-available is not an available statistics. Please choose one of: mean, median, sd"
  )
  expect_snapshot(
    .calc_tri(shp, srtmdem)
  )
  expect_snapshot(
    .calc_tri(shp, srtmdem, stats = c("mean", "median", "sd"))
  )
  expect_snapshot(
    .calc_tri(shp, srtmdem, engine = "extract")
  )
  expect_snapshot(
    .calc_tri(shp, srtmdem, engine = "exactextract")
  )
})
