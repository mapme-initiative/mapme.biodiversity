test_that("worldclim maximum temperature works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_worldclim_max_temperature(years = 2018))
  worldclim_max_temperature <- prep_resources(x)[["worldclim_max_temperature"]]

  cmx <- calc_temperature_max_wc()
  expect_true(is.null(cmx(x, NULL)))

  result <- cmx(x, worldclim_max_temperature)
  cmx <- calc_temperature_max_wc(stats = c("mean", "median", "stdev"))
  result_multi_stat <- cmx(x, worldclim_max_temperature)
  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  vars <- c("worldclim_tmax_mean", "worldclim_tmax_median", "worldclim_tmax_stdev")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_snapshot(result$value)
})
