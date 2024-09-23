test_that("calc_conflict_exposure_acled works", {
  x <- read_sf(
    system.file("extdata", "burundi.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  worldpop <- rast(system.file("extdata", "burundi_worldpop.tif",
    package = "mapme.biodiversity"
  ))
  acled <- list(read_sf(system.file("res", "acled", "acled_events_2000.gpkg",
    package = "mapme.biodiversity"
  )))

  expect_silent(calc_exposed_population_acled(distance = 1))
  expect_error(calc_exposed_population_acled(distance = c(1, 2)), "Wrong number")
  expect_silent(calc_exposed_population_acled(distance = 1:6))
  expect_error(calc_exposed_population_acled(filter_category = "other"), "arg")
  expect_error(
    calc_exposed_population_acled(
      filter_category = "event_type", filter_types = "other"
    ),
    "filter_types"
  )
  expect_silent(calc_exposed_population_acled(filter_types = c("battles", "riots")))
  expect_silent(calc_exposed_population_acled(distance = 1:2, filter_types = c("battles", "riots")))
  expect_error(
    calc_exposed_population_acled(
      distance = 1:3, filter_types = c("battles", "riots")
    ),
    "Total number"
  )
  expect_error(calc_exposed_population_acled(precision_location = 4), "precision_location")
  expect_error(calc_exposed_population_acled(precision_time = 4), "precision_time")
  expect_error(calc_exposed_population_acled(years = 1996), "years")
  expect_error(calc_exposed_population_acled(engine = "NA"), "engine")

  cce <- calc_exposed_population_acled(
    distance = 10000,
    years = 2001,
    precision_location = 3,
    precision_time = 3
  )

  expect_equal(cce(x, NULL, worldpop), NULL)
  expect_equal(cce(x, acled, NULL), NULL)
  expect_equal(cce(x, acled, worldpop), NULL)

  cce <- calc_exposed_population_acled(
    distance = 10000,
    years = 2000,
    precision_location = 3,
    precision_time = 3
  )
  result_default <- cce(x = x, acled, worldpop)
  expect_silent(.check_single_asset(result_default))
  expect_equal(nrow(result_default), 7)
  expect_equal(result_default$value[7], 1788220, tolerance = 1e-4)

  acled2 <- list(dplyr::filter(acled[[1]], event_type == "Battles"))
  cce <- calc_exposed_population_acled(
    distance = 10000,
    filter_category = "event_type",
    filter_types = "riots",
    years = 2000,
    precision_location = 3,
    precision_time = 3
  )
  expect_equal(cce(x, acled2, worldpop), NULL)

  cce <- calc_exposed_population_acled(
    distance = 10000,
    filter_category = "event_type",
    filter_types = "battles",
    years = 2000,
    precision_location = 3,
    precision_time = 3
  )
  result_battles <- cce(x, acled2, worldpop)
  expect_silent(.check_single_asset(result_battles))
  expect_equal(nrow(result_battles), 2)
  expect_equal(unique(result_battles$value), 227035, tolerance = 1e-4)

  acled2 <- acled[[1]]
  acled2$year <- 2001
  acled <- list(acled[[1]], acled2)

  cce <- calc_exposed_population_acled(
    distance = 10000,
    years = 2000:2001,
    precision_location = 3,
    precision_time = 3
  )

  result <- cce(x, acled, worldpop)
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 14)
  expect_identical(result$value[1], result$value[8])

  cce <- calc_exposed_population_acled(filter_category = "sub_event_type")
  expect_silent(result <- cce(x, acled[1], worldpop))
  expect_equal(nrow(result), 6)

  cce <- calc_exposed_population_acled(filter_category = "disorder_type")
  expect_silent(result <- cce(x, acled[1], worldpop))
  expect_equal(nrow(result), 4)

  cce <- calc_exposed_population_acled(
    distance = c(5000, 10000),
    filter_category = "event_type",
    filter_types = c("riots", "explosions/remote_violence")
  )
  expect_silent(result <- cce(x, acled[1], worldpop))
  expect_equal(nrow(result), 3)
})
