test_that("traveltime works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nelson_et_al <- list.files(system.file("res", "nelson_et_al",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nelson_et_al <- rast(nelson_et_al)

  ctt <- calc_traveltime()
  result <- ctt(shp, nelson_et_al)
  ctt <- calc_traveltime(stats = c("mean", "median", "sd"))
  result_multi_stat <- ctt(shp, nelson_et_al)
  ctt <- calc_traveltime(engine = "zonal")
  result_zonal <- ctt(shp, nelson_et_al)
  ctt <- calc_traveltime(engine = "extract")
  result_extract <- ctt(shp, nelson_et_al)
  ctt <- calc_traveltime(engine = "exactextract")
  result_exact <- ctt(shp, nelson_et_al)

  expect_equal(
    names(result),
    c("minutes_mean", "distance")
  )
  expect_equal(
    names(result_multi_stat),
    c("minutes_mean", "minutes_median", "minutes_sd", "distance")
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
    result_zonal$minutes_mean,
    result_zonal$minutes_mean,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact$minutes_mean
  )
})
