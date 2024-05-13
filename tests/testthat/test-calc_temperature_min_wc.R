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
  cmin <- calc_temperature_min_wc(stats = c("mean", "median", "sd"))
  result_multi_stat <- cmin(x, worldclim_min_temperature)
  cmin <- calc_temperature_min_wc(engine = "zonal")
  result_zonal <- cmin(x, worldclim_min_temperature)
  cmin <- calc_temperature_min_wc(engine = "extract")
  result_extract <- cmin(x, worldclim_min_temperature)
  cmin <- calc_temperature_min_wc(engine = "exactextract")
  result_exact <- cmin(x, worldclim_min_temperature)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))


  vars <- c("worldclim_tmin_mean", "worldclim_tmin_median", "worldclim_tmin_sd")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_equal(result_zonal$value, result_extract$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
