test_that("worldclim precipitation works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_worldclim_precipitation(years = 2018))
  worldclim_precipitation <- prep_resources(x)[["worldclim_precipitation"]]

  cpwc <- calc_precipitation_wc()
  result <- cpwc(x, worldclim_precipitation)
  cpwc <- calc_precipitation_wc(stats = c("mean", "median", "stdev"))
  result_multi_stat <- cpwc(x, worldclim_precipitation)
  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  vars <- c("worldclim_prec_mean", "worldclim_prec_median", "worldclim_prec_stdev")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_snapshot(result$value)
})
