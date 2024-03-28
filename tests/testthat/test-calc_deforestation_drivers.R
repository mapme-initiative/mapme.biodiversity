test_that("deforestation drivers orks", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  drivers <- list.files(
    system.file("res", "fritz_et_al",
      package = "mapme.biodiversity"
    ),
    pattern = ".tif$", full.names = TRUE
  )
  drivers <- rast(drivers)

  cdf <- calc_deforestation_drivers()
  result <- cdf(shp, drivers)

  expect_true(inherits(result, "tbl_df"))

  expect_equal(
    names(result),
    c("class", "area", "percent")
  )

  expect_equal(
    unique(result$class),
    c(
      "commercial agriculture", "commercial oil palm", "managed forests",
      "mining", "natural disturbances", "pasture", "roads", "wildfire",
      "other subsistance agriculture", "shifting cultivation"
    )
  )
  expect_equal(
    result$percent[9],
    1
  )
})
