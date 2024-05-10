test_that(".get_esalandcover works", {
  skip_on_cran()
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  # Add testing attribute in order to skip downloads
  expect_error(get_esalandcover(years = 2000))
  gel <- get_esalandcover(years = 2015)
  expect_silent(.check_resource_fun(gel))
  fps <- gel(x)
  expect_silent(.check_footprints(fps))
  # adds test to check for multiple polygons in the same tile
  x_split <- st_as_sf(st_make_grid(x, n = 2))
  fps2 <- gel(x_split)
  expect_true(identical(fps, fps2))
})
