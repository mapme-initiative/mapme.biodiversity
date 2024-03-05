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

  cpwc <- calc_precipitation_wc()
  result <- cpwc(shp, worldclim_precipitation)
  cpwc <- calc_precipitation_wc(stats = c("mean", "median", "sd"))
  result_multi_stat <- cpwc(shp, worldclim_precipitation, )
  cpwc <- calc_precipitation_wc(engine = "zonal")
  result_zonal <- cpwc(shp, worldclim_precipitation)
  cpwc <- calc_precipitation_wc(engine = "extract")
  result_extract <- cpwc(shp, worldclim_precipitation)
  cpwc <- calc_precipitation_wc(engine = "exactextract")
  result_exact <- cpwc(shp, worldclim_precipitation)

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
