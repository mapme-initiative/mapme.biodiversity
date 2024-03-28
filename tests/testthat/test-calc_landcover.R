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
  cl <- calc_landcover()
  result <- cl(shp, esalandcover)
  expect_equal(
    names(result),
    c("classes", "year", "area", "percentage")
  )
  expect_equal(
    unique(result$year),
    c("2015", "2016", "2017", "2018", "2019")
  )
})
