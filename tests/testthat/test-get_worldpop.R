test_that(".get_worldpop works", {
  skip_on_cran()

  expect_error(get_worldpop(1999))
  gwp <- get_worldpop(years = 2001)
  expect_silent(.check_resource_fun(gwp))
  fps <- gwp()
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "ppp_2001_1km_Aggregated.tif")
})
