test_that(".get_gsw works", {
  skip_on_cran()

  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  expect_error(.get_gsw(x, statistic = "not-available"))
  expect_error(.get_gsw(x, version = "not-available"))

  gsw <- get_global_surface_water_change()
  expect_silent(.check_resource_fun(gsw))
  expect_silent(fps <- gsw(x))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "change_80W_20Nv1_4_2021.tif")

  gsw <- get_global_surface_water_transitions()
  expect_silent(.check_resource_fun(gsw))
  expect_silent(fps <- gsw(x))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "transitions_80W_20Nv1_4_2021.tif")

  gsw <- get_global_surface_water_seasonality()
  expect_silent(.check_resource_fun(gsw))
  expect_silent(fps <- gsw(x))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "seasonality_80W_20Nv1_4_2021.tif")

  gsw <- get_global_surface_water_recurrence()
  expect_silent(.check_resource_fun(gsw))
  expect_silent(fps <- gsw(x))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "recurrence_80W_20Nv1_4_2021.tif")

  gsw <- get_global_surface_water_occurrence()
  expect_silent(.check_resource_fun(gsw))
  expect_silent(fps <- gsw(x))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "occurrence_80W_20Nv1_4_2021.tif")

  # adds test to check for multiple polygons in the same tile
  x_split <- st_as_sf(st_make_grid(x, n = 2))
  expect_silent(fps <- .get_gsw(x_split, statistic = "occurrence"))
  expect_equal(nrow(fps), 1)
})
