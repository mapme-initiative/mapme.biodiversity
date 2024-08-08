test_that("conflict_exposure works", {
  x <- read_sf(
    system.file("extdata", "burundi.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  worldpop <- rast(system.file("extdata", "burundi_worldpop.tif",
    package = "mapme.biodiversity"
  ))
  ucdp_ged <- list(read_sf(system.file("res", "ucdp_ged", "ged221-csv.gpkg",
    package = "mapme.biodiversity"
  )))

  expect_error(calc_exposed_population(distance = -1), "distance")
  expect_error(calc_exposed_population(distance = c(1, 2)), "distance")
  expect_error(calc_exposed_population(violence_types = 4, "violence_type"))
  expect_error(calc_exposed_population(years = 1988), "years")
  expect_error(calc_exposed_population(precision_location = 8), "precision_location")
  expect_error(calc_exposed_population(precision_time = 8), "precision_time")
  expect_error(calc_exposed_population(engine = "NA"), "engine")


  cce <- calc_exposed_population(
    distance = 10000,
    violence_types = 1:3,
    years = 2000,
    precision_location = 4,
    precision_time = 4
  )

  expect_equal(cce(x, NULL, worldpop), NULL)
  expect_equal(cce(x, ucdp_ged, NULL), NULL)
  expect_equal(cce(x, ucdp_ged, worldpop), NULL)

  cce <- calc_exposed_population(
    distance = 10000,
    violence_types = 1:3,
    years = 1991:1992,
    precision_location = 4,
    precision_time = 4
  )


  ucdp2 <- list(ucdp_ged[[1]][12:16, ])
  expect_equal(cce(x, ucdp2, worldpop), NULL)
  result <- cce(x, ucdp_ged, worldpop)
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$value, c(875551, 124199))


  ucdp_ged[[1]]$date_start <- as.POSIXct(ucdp_ged[[1]]$date_start) + 14 * 365 * 24 * 60 * 60

  worldpop <- c(worldpop, round(worldpop * 1.15))
  names(worldpop) <- c(
    "ppp_2004_1km_Aggregated",
    "ppp_2005_1km_Aggregated"
  )

  cce <- calc_exposed_population(
    distance = 10000,
    years = 2005:2006,
    precision_location = 4,
    precision_time = 4
  )
  result <- cce(x, ucdp_ged, worldpop)
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$value, c(1006881, 142832))
})
