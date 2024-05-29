test_that("calc_burned_area works", {
  skip_on_cran()
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_mcd64a1(years = 2010))
  mcd64a1 <- prep_resources(x)[["mcd64a1"]]

  ba <- calc_burned_area()
  expect_true(is.null(ba(x, NULL)))
  result <- ba(x, mcd64a1)
  ba <- calc_burned_area(engine = "zonal")
  result_zonal <- ba(x, mcd64a1)
  ba <- calc_burned_area(engine = "extract")
  result_extract <- ba(x, mcd64a1)
  ba <- calc_burned_area(engine = "exactextract")
  result_exact <- ba(x, mcd64a1)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  expect_equal(result_zonal$value, result_zonal$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
