test_that(".calc_fatalities works", {
  shp <- read_sf(
    system.file("extdata", "burundi.gpkg",
      package = "mapme.biodiversity"
    )
  )
  ucdp_ged <- list.files(system.file("res", "ucdp_ged",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  ucdp_ged <- read_sf(ucdp_ged)
  attributes(shp)$years <- 1980

  expect_equal(
    .calc_fatalities(shp, ucdp_ged[0, ]),
    NA
  )

  expect_error(
    .calc_fatalities(shp, list(ucdp_ged)),
    "The target years do not intersect with the availability of ucdp_ged."
  )
  attributes(shp)$years <- 1991:1992

  expect_error(
    .calc_fatalities(shp, list(ucdp_ged), precision_location = 8),
    "Argument precision_location must be a single numeric between 1 and 7."
  )

  expect_error(
    .calc_fatalities(shp, list(ucdp_ged), precision_time = 6),
    "Argument precision_time must be a single numeric between 1 and 5."
  )

  result_default <- .calc_fatalities(shp, list(ucdp_ged))
  result_all <- .calc_fatalities(shp, list(ucdp_ged), precision_location = 7, precision_time = 5)

  cols <- c("month", "type_of_violence", "deaths_civilians", "deaths_unknown", "deaths_total", "event_count")
  n_rows <- 72

  expect_equal(
    names(result_default),
    cols
  )

  expect_equal(
    nrow(result_default),
    n_rows
  )

  expect_equal(
    sum(result_default$deaths_total),
    20
  )

  expect_equal(
    sum(result_default$event_count),
    1
  )

  expect_equal(
    names(result_all),
    cols
  )

  expect_equal(
    nrow(result_all),
    n_rows
  )

  expect_equal(
    sum(result_all$deaths_total),
    403
  )

  expect_equal(
    sum(result_all$event_count),
    22
  )
})
