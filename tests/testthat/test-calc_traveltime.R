test_that("traveltime works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nelson_et_al <- list.files(system.file("res", "nelson_et_al",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nelson_et_al <- rast(nelson_et_al)

  ctt <- calc_traveltime()
  expect_true(is.null(ctt(x, NULL)))
  result <- ctt(x, nelson_et_al)
  ctt <- calc_traveltime(stats = c("mean", "median", "sd"))
  result_multi_stat <- ctt(x, nelson_et_al)
  ctt <- calc_traveltime(engine = "zonal")
  result_zonal <- ctt(x, nelson_et_al)
  ctt <- calc_traveltime(engine = "extract")
  result_extract <- ctt(x, nelson_et_al)
  ctt <- calc_traveltime(engine = "exactextract")
  result_exact <- ctt(x, nelson_et_al)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  vars <- c(
    "100k_200k_traveltime_mean", "100k_200k_traveltime_median",
    "100k_200k_traveltime_sd"
  )
  expect_equal(unique(result_multi_stat$variable)[1:3], vars)
  expect_equal(result_zonal$value, result_zonal$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
