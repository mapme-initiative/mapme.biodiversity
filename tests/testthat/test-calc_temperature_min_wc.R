test_that("worldclim minimum temperature works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  worldclim_min_temperature <- list.files(system.file("res", "worldclim_min_temperature",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  worldclim_min_temperature <- rast(worldclim_min_temperature)

  cmin <- calc_temperature_min_wc()
  result <- cmin(shp, worldclim_min_temperature)
  cmin <- calc_temperature_min_wc(stats = c("mean", "median", "sd"))
  result_multi_stat <- cmin(shp, worldclim_min_temperature)
  cmin <- calc_temperature_min_wc(engine = "zonal")
  result_zonal <- cmin(shp, worldclim_min_temperature)
  cmin <- calc_temperature_min_wc(engine = "extract")
  result_extract <- cmin(shp, worldclim_min_temperature)
  cmin <- calc_temperature_min_wc(engine = "exactextract")
  result_exact <- cmin(shp, worldclim_min_temperature)

  expect_equal(
    names(result),
    c("tmin_mean", "date")
  )
  expect_equal(
    names(result_multi_stat),
    c("tmin_mean", "tmin_median", "tmin_sd", "date")
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
    result_zonal$tmin_mean,
    result_extract$tmin_mean,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact$tmin_mean
  )
})
