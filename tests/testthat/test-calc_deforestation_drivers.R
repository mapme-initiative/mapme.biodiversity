test_that("deforestation drivers works", {
  x <- read_sf(
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
  result <- cdf(x, drivers)
  expect_silent(.check_single_asset(result))
  expect_equal(
    unique(result$variable),
    c(
      "commercial_agriculture", "commercial_oil_palm", "managed_forests",
      "mining", "natural_disturbances", "pasture", "roads", "wildfire",
      "other_subsistance_agriculture", "shifting_cultivation"
    )
  )
  expect_equal(sum(result$value), 16209.58, tolerance = 1e-4)
})
