test_that("get_soilgrids works", {
  skip_on_cran()
  skip_if_not(Sys.getenv("USER") == "darius")

  expect_error(get_soilgrids(layers = "na", depths = .sg_depths, stats = .sg_stats))
  expect_error(get_soilgrids(layers = names(.sg_layers), depths = "na", stats = .sg_stats))
  expect_error(get_soilgrids(layers = names(.sg_layers), depths = .sg_depths, stats = "na"))

  gsg <- get_soilgrids(layers = names(.sg_layers)[1], depths = .sg_depths[1], stats = .sg_stats[1])
  expect_silent(.check_resource_fun(gsg))
  suppressWarnings(fps <- gsg())
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "bdod_0-5cm_Q0.05.vrt")
})
