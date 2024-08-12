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
  cdi <- calc_drought_indicator(stats = c("mean", "median", "stdev"))
  result_multi_stats <- cdi(x, nasa_grace)
  expect_silent(.check_single_asset(result[[1]]))
  expect_equal(
    unique(result_multi_stats[[1]]$variable),
    c("wetness_mean", "wetness_median", "wetness_stdev")
  )
  expect_snapshot(
    result[[1]]$value
  )
})
