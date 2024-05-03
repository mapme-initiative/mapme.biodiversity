test_that("soilpoperties works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- suppressWarnings(st_cast(x, to = "POLYGON"))[1, ]

  soilgrids <- list.files(system.file("res", "soilgrids",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)

  soilgrids <- rast(soilgrids)

  csp <- calc_soilproperties()
  expect_true(is.null(csp(x, NULL)))

  csp <- calc_soilproperties()
  result <- csp(x, soilgrids)
  csp <- calc_soilproperties(engine = "extract", stats = c("mean", "median", "sd"))
  result_multi_stat <- csp(x, soilgrids)
  csp <- calc_soilproperties(engine = "zonal")
  result_zonal <- csp(x, soilgrids)
  csp <- calc_soilproperties(engine = "extract")
  result_extract <- csp(x, soilgrids)
  csp <- calc_soilproperties(engine = "exactextract")
  result_exact <- csp(x, soilgrids)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))
  expect_equal(result_zonal$value, result_extract$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
