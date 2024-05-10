test_that(".get_teow works", {
  skip_on_cran()

  gteow <- get_teow()
  expect_silent(.check_resource_fun(gteow))
  fps <- gteow()
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "wwf_terr_ecos.gpkg")
})
