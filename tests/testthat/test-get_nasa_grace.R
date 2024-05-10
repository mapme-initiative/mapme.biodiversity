test_that(".get_droughtind works", {
  skip_on_cran()

  expect_error(get_nasa_grace(2000))
  gng <- get_nasa_grace(years = 2004:2010)
  expect_silent(.check_resource_fun(gng))
  expect_silent(fps <- gng())
  expect_silent(.check_footprints(fps))
  expect_equal(nrow(fps), 365)
})
