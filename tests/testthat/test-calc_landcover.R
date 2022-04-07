test_that("esa global landcover works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  esalandcover <- list.files(system.file("res", "esalandcover",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  esalandcover <- rast(esalandcover)
  attributes(shp)$years <- 2015:2020
  attributes(shp)$cores <- 1
  expect_snapshot(
    .calc_landcover(shp, esalandcover)
  )
})
