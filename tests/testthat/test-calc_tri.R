test_that("terrain ruggedness index works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nasa_srtm <- list.files(system.file("res", "nasa_srtm",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nasa_srtm <- rast(nasa_srtm)

  ctri <- calc_tri()
  expect_true(is.null(ctri(x, NULL)))
  result <- ctri(x, nasa_srtm)
  ctri <- calc_tri(stats = c("mean", "median", "sd"))
  result_multi_stat <- ctri(x, nasa_srtm)
  ctri <- calc_tri(engine = "zonal")
  result_zonal <- ctri(x, nasa_srtm)
  ctri <- calc_tri(engine = "extract")
  result_extract <- ctri(x, nasa_srtm)
  ctri <- calc_tri(engine = "exactextract")
  result_exact <- ctri(x, nasa_srtm)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  vars <- c("tri_mean", "tri_median", "tri_sd")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_equal(result_zonal$value, result_extract$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
