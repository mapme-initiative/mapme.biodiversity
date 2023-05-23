test_that("active fire properties works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
      package = "mapme.biodiversity"
    )
  )
  source <- list.files(system.file("res", "nasa_firms",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  nasa_firms <- read_sf(source)
  result <- .calc_active_fire_properties(shp, list(nasa_firms))
  expect_equal(
    nrow(result),
    21
  )

  nasa_firms2 <- list(nasa_firms, nasa_firms)
  nasa_firms2[[2]]$instrument <- "MODIS"
  nasa_firms2[[2]]$confidence <- 1:nrow(nasa_firms2[[2]])
  result2 <- .calc_active_fire_properties(shp, nasa_firms2)
  expect_equal(
    nrow(result2),
    42
  )
})
