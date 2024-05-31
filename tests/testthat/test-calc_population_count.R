test_that(".calc_population_count works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_worldpop(years = 2010:2020))
  worldpop <- prep_resources(x)[["worldpop"]]

  cpc <- calc_population_count()
  result <- cpc(x, worldpop)
  cpc <- calc_population_count(stats = c("mean", "median", "sd"))
  result_multi_stat <- cpc(x, worldpop)
  cpc <- calc_population_count(engine = "zonal")
  result_zonal <- cpc(x, worldpop)
  cpc <- calc_population_count(engine = "extract")
  result_extract <- cpc(x, worldpop)
  cpc <- calc_population_count(engine = "exactextract")
  result_exact <- cpc(x, worldpop)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  vars <- c("population_mean", "population_median", "population_sd")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_equal(result_zonal$value, result_extract$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
