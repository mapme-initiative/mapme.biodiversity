test_that("traveltime_2000 works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
                package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme-data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_accessibility_2000())
  accessibility_2000 <- prep_resources(x)[["accessibility_2000"]]

  ctt_2000 <- calc_traveltime_2000()
  expect_true(is.null(ctt_2000(x, NULL)))
  result <- ctt_2000(x, accessibility_2000)
  ctt_2000 <- calc_traveltime_2000(stats = c("mean", "median", "sd"))
  result_multi_stat <- ctt_2000(x, accessibility_2000)
  ctt_2000 <- calc_traveltime_2000(engine = "zonal")
  result_zonal <- ctt_2000(x, accessibility_2000)
  ctt_2000 <- calc_traveltime_2000(engine = "extract")
  result_extract <- ctt_2000(x, accessibility_2000)
  ctt_2000 <- calc_traveltime_2000(engine = "exactextract")
  result_exact <- ctt_2000(x, accessibility_2000)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  vars <- c("traveltime_2000_mean", "traveltime_2000_median", "traveltime_2000_sd")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_equal(result_zonal$value, result_extract$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
