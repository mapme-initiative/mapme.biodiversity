test_that("esa global landcover works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  esalandcover <- list.files(system.file("res", "esalandcover",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  esalandcover <- rast(esalandcover)
  cl <- calc_landcover()
  expect_true(is.null(cl(x, NULL)))
  result <- cl(x, esalandcover)
  expect_silent(.check_single_asset(result))
})
