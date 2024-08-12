test_that("precipitation indicator works", {
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_chelsa(years = 2010))
  chelsa <- prep_resources(x)[["chelsa"]]

  expect_message(cpc <- calc_precipitation_chelsa(1978:1982))
  expect_error(calc_precipitation_chelsa(years = 1970:1971))
  cpc <- calc_precipitation_chelsa(years = 2010)
  result <- cpc(x, chelsa)
  expect_silent(.check_single_asset(result))
  expect_equal(unique(result$variable), "precipitation")
  expect_snapshot(result$value)
})
