# test-get_ucdp_ged.R
test_that(".get_ucdp_ged works", {
  skip_on_cran()

  expect_error(get_ucdp_ged(version = "not-available"))

  guc <- get_ucdp_ged(version = "latest")
  expect_silent(.check_resource_fun(guc))
  fps <- guc()
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "ged241-csv.gpkg")

  guc <- get_ucdp_ged(version = "19.1")
  fps <- guc()
  expect_equal(fps$filename, "ged191-csv.gpkg")

  expect_equal(
    .ucdp_versions(),
    c(
      "5.0", "17.1", "17.2", "18.1", "19.1",
      "20.1", "21.1", "22.1", "23.1", "24.1"
    )
  )
})
