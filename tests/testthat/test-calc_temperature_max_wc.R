test_that("worldclim maximum temperature works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  worldclim_max_temperature <- list.files(system.file("res", "worldclim_max_temperature",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  worldclim_max_temperature <- rast(worldclim_max_temperature)

  cmx <- calc_temperature_max_wc()
  result <- cmx(shp, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(stats = c("mean", "median", "sd"))
  result_multi_stat <- cmx(shp, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(engine = "zonal")
  result_zonal <- cmx(shp, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(engine = "extract")
  result_extract <- cmx(shp, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(engine = "exactextract")
  result_exact <- cmx(shp, worldclim_max_temperature)

  expect_equal(
    names(result),
    c("tmax_mean", "date")
  )
  expect_equal(
    names(result_multi_stat),
    c("tmax_mean", "tmax_median", "tmax_sd", "date")
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
    result_zonal$tmax_mean,
    result_extract$tmax_mean,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact$tmax_mean
  )
})
