test_that("precipitation indicator works", {
  skip_on_cran()
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_chirps(years = 2010))
  chirps <- prep_resources(x)[["chirps"]]

  expect_message(cpc <- calc_precipitation_chirps(1980:1982))
  expect_error(calc_precipitation_chirps(years = 1970:1971))
  expect_true(is.null(cpc(x, NULL)))
  cpc <- calc_precipitation_chirps(years = 2010)
  result <- cpc(x, chirps)
  expect_silent(.check_single_asset(result[[1]]))
  expect_equal(unique(result[[1]]$variable), "precipitation")
  expect_snapshot(result[[1]]$value)
})
