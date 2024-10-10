test_that("conflict_exposure_ucdp works", {
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

  expect_silent(calc_exposed_population_ucdp(distance = 1))
  expect_error(calc_exposed_population_ucdp(distance = c(1, 2)), "distance")
  expect_silent(calc_exposed_population_ucdp(violence_types = 1:2, distance = c(1, 2)))
  expect_error(calc_exposed_population_ucdp(violence_types = 4, "violence_type"))
  expect_error(calc_exposed_population_ucdp(years = 1988), "years")
  expect_error(calc_exposed_population_ucdp(precision_location = 8), "precision_location")
  expect_error(calc_exposed_population_ucdp(precision_time = 8), "precision_time")
  expect_error(calc_exposed_population_ucdp(engine = "NA"), "engine")

  cce <- calc_exposed_population_ucdp(
    distance = 10000,
    violence_types = 1:3,
    years = 2000,
    precision_location = 4,
    precision_time = 4
  )

  expect_equal(cce(x, NULL, worldpop), NULL)
  expect_equal(cce(x, ucdp_ged, NULL), NULL)
  expect_equal(cce(x, ucdp_ged, worldpop), NULL)

  cce <- calc_exposed_population_ucdp(
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
  expect_equal(nrow(result), 5)
  expect_equal(result$value[c(3, 5)], c(873211, 123397), tolerance = 1e-4)

  ucdp_ged[[1]]$date_start <- as.POSIXct(ucdp_ged[[1]]$date_start) + 14 * 365 * 24 * 60 * 60

  worldpop <- c(worldpop, round(worldpop * 1.15))
  names(worldpop) <- c(
    "ppp_2004_1km_Aggregated",
    "ppp_2005_1km_Aggregated"
  )

  cce <- calc_exposed_population_ucdp(
    distance = 10000,
    years = 2005:2006,
    precision_location = 4,
    precision_time = 4
  )
  result <- cce(x, ucdp_ged, worldpop)
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 5)
  expect_equal(result$value[c(3, 5)], c(1004191, 141910), tolerance = 1e-4)

  cce <- calc_exposed_population_ucdp(
    distance = c(10000, 10000),
    violence_types = c(1, 3),
    years = 2005:2006,
    precision_location = 4,
    precision_time = 4
  )

  result <- cce(x, ucdp_ged, worldpop)
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 5)
  expect_equal(result$value[c(3, 5)], c(1004191, 141910), tolerance = 1e-4)
})
