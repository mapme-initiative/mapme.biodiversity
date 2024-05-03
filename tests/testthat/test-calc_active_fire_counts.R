test_that("active fire count works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
      package = "mapme.biodiversity"
    )
  )
  source <- list.files(system.file("res", "nasa_firms",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)

  nasa_firms <- read_sf(source)
  afc <- calc_active_fire_counts()
  expect_true(is.null(afc(x, NULL)))
  result <- afc(x, list(nasa_firms))
  expect_silent(.check_single_asset(result))
  expect_true(inherits(result$datetime, "Date"))
  expect_equal(result$value, 21)

  # test that it works with both VIRRS and MODIS sensors
  nasa_firms2 <- list(nasa_firms, nasa_firms)
  nasa_firms2[[2]]$instrument <- "MODIS"
  result2 <- afc(x, nasa_firms2)
  expect_silent(.check_single_asset(result2))
  expect_equal(nrow(result2), 2)
  expect_equal(result2$variable, c("modis_fire_count", "viirs_fire_count"))
})
