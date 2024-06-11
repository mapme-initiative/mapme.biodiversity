test_that(".get_nasa_srtm works", {
  skip_on_cran()

  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  gns <- get_nasa_srtm()
  expect_silent(.check_resource_fun(gns))
  fps <- gns(x)
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "NASADEM_HGT_n18w072.tif")
  x_split <- st_as_sf(st_make_grid(x, n = 2))
  fps <- gns(x_split)
  expect_equal(nrow(fps), 1)
})
