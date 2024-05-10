test_that(".get_gmw works", {
  skip_on_cran()

  expect_error(get_gmw(2011))
  expect_silent(gmw <- get_gmw(years = 2007:2008))
  expect_silent(.check_resource_fun(gmw))
  expect_silent(fps <- gmw())
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, c("gmw_v3_2007_vec.gpkg", "gmw_v3_2008_vec.gpkg"))
})
