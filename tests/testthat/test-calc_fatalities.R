test_that(".calc_fatalities works", {
  x <- read_sf(
    system.file("extdata", "burundi.gpkg",
      package = "mapme.biodiversity"
    )
  )
  ucdp_ged <- list.files(system.file("res", "ucdp_ged",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  ucdp_ged <- read_sf(ucdp_ged)

  expect_error(
    calc_fatalities(years = 1980),
    "The target years do not intersect with the availability of ucdp_ged."
  )
  expect_error(
    calc_fatalities(precision_location = 8),
    "Argument precision_location must be a single numeric between 1 and 7."
  )
  expect_error(
    calc_fatalities(precision_time = 6),
    "Argument precision_time must be a single numeric between 1 and 5."
  )

  cf <- calc_fatalities(years = 1991:1992)
  result_default <- cf(x, list(ucdp_ged))
  cf <- calc_fatalities(years = 1991:1992, precision_location = 7, precision_time = 5)
  result_all <- cf(x, list(ucdp_ged))

  conf_types <- c("state_based_conflict", "non_state_conflict", "one_sided_violence")
  death_types <- c("deaths_civilians", "deaths_unknown", "deaths_total")
  vars <- paste(rep(conf_types, each = length(death_types)), death_types, sep = "_")
  n_rows <- 216

  expect_silent(.check_single_asset(result_default))
  expect_equal(unique(result_default$variable), vars)
  expect_equal(nrow(result_default), n_rows)
  expect_equal(sum(result_default$value), 40)

  expect_equal(unique(result_all$variable), vars)
  expect_equal(nrow(result_all), n_rows)
  expect_equal(sum(result_all$value), 673)
})
