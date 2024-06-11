test_that(".get_nelson_et_al works", {
  skip_on_cran()

  expect_error(get_nelson_et_al(ranges = "na"))
  gne <- get_nelson_et_al(ranges = "20k_50k")
  expect_silent(.check_resource_fun(gne))
  fps <- gne()
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "traveltime-20k_50k.tif")
})
