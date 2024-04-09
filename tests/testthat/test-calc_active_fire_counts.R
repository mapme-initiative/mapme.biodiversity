test_that("active fire count works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
      package = "mapme.biodiversity"
    )
  )
  source <- list.files(system.file("res", "nasa_firms",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  nasa_firms <- read_sf(source)
  afc <- calc_active_fire_counts()
  result <- afc(shp, list(nasa_firms))
  expect_equal(
    names(result),
    c("instrument", "year", "active_fire_counts")
  )
  expect_equal(
    result$instrument,
    "VIIRS"
  )
  expect_equal(
    result$year,
    "2021"
  )
  expect_equal(
    result$active_fire_counts,
    21
  )

  # test that it works with both VIRRS and MODIS sensors
  nasa_firms2 <- list(nasa_firms, nasa_firms)
  nasa_firms2[[2]]$instrument <- "MODIS"
  result2 <- afc(shp, nasa_firms2)
  expect_equal(
    result2$instrument,
    c("MODIS", "VIIRS")
  )
})
