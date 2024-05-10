test_that(".get_fritz_et_al works", {
  skip_on_cran()

  expect_error(get_fritz_et_al(resolution = 1))
  gf <- get_fritz_et_al(resolution = 100)
  expect_silent(.check_resource_fun(gf))
  suppressWarnings(fps <- gf())
  expect_silent(.check_footprints(fps))
  gf <- get_fritz_et_al(resolution = 1000)
  suppressWarnings(fps2 <- gf())
  expect_silent(.check_footprints(fps2))
  expect_equal(fps$filename, "geo_fritz_et_al_100m.tif")
  expect_equal(fps2$filename, "geo_fritz_et_al_1000m.tif")
})
