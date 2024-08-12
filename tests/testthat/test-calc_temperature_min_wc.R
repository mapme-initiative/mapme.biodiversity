test_that("worldclim minimum temperature works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_worldclim_min_temperature(years = 2018))
  worldclim_min_temperature <- prep_resources(x)[["worldclim_min_temperature"]]

  cmin <- calc_temperature_min_wc()
  expect_true(is.null(cmin(x, NULL)))
  result <- cmin(x, worldclim_min_temperature)
  cmin <- calc_temperature_min_wc(stats = c("mean", "median", "stdev"))
  result_multi_stat <- cmin(x, worldclim_min_temperature)
  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  vars <- c("worldclim_tmin_mean", "worldclim_tmin_median", "worldclim_tmin_stdev")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_snapshot(result$value)
})
