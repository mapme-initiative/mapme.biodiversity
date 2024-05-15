test_that("get_mcd64a1 works", {
  skip_on_cran()

  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
      package = "mapme.biodiversity"
    )
  )

  gmc <- get_mcd64a1(years = 2010)
  expect_silent(.check_resource_fun(gmc))
  fps <- gmc(x)
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename[1], "MCD64A1.A2010335.h11v07.061.2021309002413_Burn_Date.tif")
  x_split <- st_as_sf(st_make_grid(x, n = 2))
  fps <- gmc(x_split)
  expect_equal(nrow(fps), 12)
})
