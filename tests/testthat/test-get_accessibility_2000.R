test_that(".get_accessibility_2000 works", {

  # Run the function and check that it returns a valid function
  ga2000 <- get_accessibility_2000()
  expect_silent(.check_resource_fun(ga2000))

  # Execute the returned function to get the footprint
  fps <- ga2000()

  # Check that the footprint is a valid sf object
  expect_silent(.check_footprints(fps))

  # Verify the filename in the returned footprint
  expect_equal(fps$filename, "acc_50k.tif")
})
