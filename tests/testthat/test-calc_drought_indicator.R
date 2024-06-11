test_that("drought indicator works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_nasa_grace(years = 2022))
  nasa_grace <- prep_resources(x)[["nasa_grace"]]

  cdi <- calc_drought_indicator()
  result <- cdi(x, nasa_grace)
  cdi <- calc_drought_indicator(stats = c("mean", "median", "sd"))
  result_multi_stats <- cdi(x, nasa_grace)
  cdi <- calc_drought_indicator(engine = "zonal")
  result_zonal <- cdi(x, nasa_grace)
  cdi <- calc_drought_indicator(engine = "extract")
  result_extract <- cdi(x, nasa_grace)
  cdi <- calc_drought_indicator(engine = "exactextract")
  result_exact <- cdi(x, nasa_grace)

  expect_silent(.check_single_asset(result[[1]]))
  expect_equal(
    unique(result_multi_stats[[1]]$variable),
    c("wetness_mean", "wetness_median", "wetness_sd")
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
    result[[1]]$value,
    result_zonal[[1]]$value,
    tolerance = 1e-4
  )
  expect_equal(
    result_zonal[[1]]$value,
    result_extract[[1]]$value,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact[[1]]$value
  )
})
