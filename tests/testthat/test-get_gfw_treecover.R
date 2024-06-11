test_that(".get_gfw_tile_id works", {
  gfw_grid <- make_global_grid()
  expect_equal(.get_gfw_tile_id(gfw_grid[100, ]), "20S_110E")
})

test_that(".get_gfw_treecover works", {
  skip_on_cran()

  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  expect_error(get_gfw_treecover("na"))
  gt <- get_gfw_treecover()
  expect_silent(.check_resource_fun(gt))
  expect_silent(fps <- gt(x))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "Hansen_GFC-2023-v1.11_treecover2000_20N_080W.tif")
})
