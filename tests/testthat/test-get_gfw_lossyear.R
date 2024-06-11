test_that(".get_gfw_lossyear works", {
  skip_on_cran()
  .clear_resources()

  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  expect_error(get_gfw_lossyear("na"))
  gl <- get_gfw_lossyear()
  expect_silent(.check_resource_fun(gl))
  expect_silent(fps <- gl(x))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "Hansen_GFC-2023-v1.11_lossyear_20N_080W.tif")
})
