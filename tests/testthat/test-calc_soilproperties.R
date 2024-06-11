test_that("soilpoperties works", {
  skip_on_cran()
  skip_if_not(Sys.getenv("USER") == "darius")

  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  mapme_options(outdir = NULL, verbose = FALSE)
  suppressWarnings(get_resources(x, get_soilgrids(
    layer = "clay",
    depth = "0-5cm",
    stat = "mean"
  )))
  soilgrids <- prep_resources(x)[["soilgrids"]]
  x <- st_transform(x, st_crs(soilgrids))

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
