test_that("worldclim precipitation works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  worldclim_precipitation <- list.files(system.file("res", "worldclim_precipitation",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  worldclim_precipitation <- rast(worldclim_precipitation)
  attributes(shp)$years <- 2000:2022
  expect_error(
    .calc_precipitation_wc(shp, worldclim_precipitation, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_precipitation_wc(shp, worldclim_precipitation, stats_worldclim = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )

  result <- .calc_precipitation_wc(shp, worldclim_precipitation)
  result_multi_stat <- .calc_precipitation_wc(shp, worldclim_precipitation, stats_worldclim = c("mean", "median", "sd"))
  result_zonal <- .calc_precipitation_wc(shp, worldclim_precipitation, engine = "zonal")
  result_extract <- .calc_precipitation_wc(shp, worldclim_precipitation, engine = "extract")
  result_exact <- .calc_precipitation_wc(shp, worldclim_precipitation, engine = "exactextract")

  expect_equal(
    names(result),
    c("prec_mean", "date")
  )
  expect_equal(
    names(result_multi_stat),
    c("prec_mean", "prec_median", "prec_sd", "date")
  )
  expect_equal(
    names(result_zonal),
    names(result_extract)
  )
  expect_equal(
    names(result_zonal),
    names(result_exact)
  )
  expect_equal(
    result_zonal$prec_mean,
    result_extract$prec_mean,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact$prec_mean
  )
})
