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
  ctri <- calc_tri(stats = c("mean", "median", "stdev"))
  result_multi_stat <- ctri(x, nasa_srtm)
  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))

  vars <- c("tri_mean", "tri_median", "tri_stdev")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_snapshot(result$value)
})
