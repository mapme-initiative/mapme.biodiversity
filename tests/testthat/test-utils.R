test_that("make_global_grid works", {
  standard_grid <- make_global_grid()
  nrows <- nrow(standard_grid)
  bbox <- st_bbox(standard_grid)
  points <- st_multipoint(matrix(c(-180, -50, -180, 80, 170, 80, 170, -50), ncol = 2, byrow = TRUE))
  bbox_c <- st_bbox(points)
  st_crs(bbox_c) <- st_crs("EPSG:4326")
  expect_equal(nrows, 455)
  expect_equal(bbox, bbox_c)
})

test_that(".check_available_years works", {
  expect_equal(
    check_available_years(2000:2020, 2000:2020, indicator = "treecover_area"),
    2000:2020
  )
  expect_equal(
    check_available_years(2000:2010, 2000:2020, indicator = "treecover_area"),
    2000:2010
  )
  expect_error(
    check_available_years(2000:2010, 2011:2020, indicator = "treecover_area"),
    "The target years do not intersect with the availability of treecover."
  )
  expect_message(
    out <- check_available_years(2000:2011, 2011:2020, indicator = "treecover_area"),
    "Some target years are not available for treecover."
  )
  expect_equal(out, 2011)
})

test_that(".check_namespace works", {
  expect_error(check_namespace("not-a-package"))
  expect_message(check_namespace("not-a-package", error = FALSE))
  expect_silent(check_namespace("base"))
})
