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
  expect_error(calc_precipitation_chelsa(years = 1981:1982, engine = "not-av"))
  expect_true(is.null(cpc(x, NULL)))
  cpc <- calc_precipitation_chelsa(years = 2010)
  result <- cpc(x, chelsa)
  cpc <- calc_precipitation_chelsa(years = 2010, engine = "zonal")
  result_zonal <- cpc(x, chelsa)
  cpc <- calc_precipitation_chelsa(years = 2010, engine = "extract")
  result_extract <- cpc(x, chelsa)
  cpc <- calc_precipitation_chelsa(years = 2010, engine = "exactextract")
  result_exact <- cpc(x, chelsa)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  expect_equal(unique(result$variable), "precipitation")
  expect_equal(result_zonal$value, result_extract$value,
    tolerance = 1e-4
  )
  expect_snapshot(result$value)
})
