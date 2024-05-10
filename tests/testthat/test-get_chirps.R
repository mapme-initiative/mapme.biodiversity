test_that(".get_chirps works", {
  skip_on_cran()

  expect_error(get_chirps(1970))
  gc <- get_chirps(years = 2010)
  expect_silent(.check_resource_fun(gc))
  fps <- gc(aoi)
  expect_silent(fps <- .check_footprints(fps))
  expect_equal(nrow(fps), 12)
})
