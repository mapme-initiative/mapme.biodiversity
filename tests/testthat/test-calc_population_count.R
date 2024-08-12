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
  cpc <- calc_population_count(stats = c("mean", "median", "stdev"))
  result_multi_stat <- cpc(x, worldpop)
  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  vars <- c("population_mean", "population_median", "population_stdev")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_snapshot(result$value)
})
