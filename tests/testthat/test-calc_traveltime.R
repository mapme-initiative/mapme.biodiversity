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
  attributes(shp)$years <- 2015
  expect_error(
    .calc_traveltime(shp, nelson_et_al, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_traveltime(shp, nelson_et_al, stats = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )

  result <- .calc_traveltime(shp, nelson_et_al)
  result_multi_stat <- .calc_traveltime(shp, nelson_et_al, stats = c("mean", "median", "sd"))
  result_zonal <- .calc_traveltime(shp, nelson_et_al, engine = "zonal")
  result_extract <- .calc_traveltime(shp, nelson_et_al, engine = "extract")
  result_exact <- .calc_traveltime(shp, nelson_et_al, engine = "exactextract")

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
