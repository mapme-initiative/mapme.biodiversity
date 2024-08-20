test_that("calc_hfp works", {
  skip_on_cran()
  x <- read_sf(system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
    package = "mapme.biodiversity"
  ))
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  get_resources(x, get_humanfootprint(years = 2010))
  hfp <- prep_resources(x)[["humanfootprint"]]
  hf <- calc_humanfootprint(stats = c("mean", "median"))

  expect_silent(result <- hf(x, hfp))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, c("humanfootprint_mean", "humanfootprint_median"))
  expect_equal(result$value, c(2.87, 2.62), tolerance = 0.01)
})
