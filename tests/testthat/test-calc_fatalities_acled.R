test_that("calc_fatalitites_acled works", {
  x <- read_sf(
    system.file("extdata", "burundi.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  get_resources(x, get_acled(
    years = 2000,
    key = "my-key",
    email = "myemail@com",
    accept_terms = TRUE
  ))

  acled <- prep_resources(x)[["acled"]][[1]]

  expect_error(
    calc_fatalities_acled(years = 1980),
    "The target years do not intersect with the availability of acled"
  )
  expect_error(
    calc_fatalities_acled(precision_location = 4),
    "Argument precision_location must be a single numeric between 1 and 3."
  )
  expect_error(
    calc_fatalities_acled(precision_time = 4),
    "Argument precision_time must be a single numeric between 1 and 3."
  )

  expect_error(
    calc_fatalities_acled(stratum = "other"),
    'should be one of "event_type", "sub_event_type", "disorder_type"'
  )

  cf <- calc_fatalities_acled(years = 2000)
  expect_equal(cf(x, NULL), NULL)
  cf <- calc_fatalities_acled(years = 2001)
  expect_equal(cf(x, list(acled)), NULL)
  cf <- calc_fatalities_acled(years = 2000, stratum = "event_type")
  result_event <- cf(x, list(acled))
  cf <- calc_fatalities_acled(years = 2000, stratum = "sub_event_type")
  result_sub <- cf(x, list(acled))
  cf <- calc_fatalities_acled(years = 2000, stratum = "disorder_type")
  result_disorder <- cf(x, list(acled))

  expect_silent(.check_single_asset(result_event))
  expect_silent(.check_single_asset(result_sub))
  expect_silent(.check_single_asset(result_disorder))

  expect_equal(sum(result_event$value), sum(result_sub$value))
  expect_equal(sum(result_event$value), sum(result_disorder$value))

  events <- c(
    "fatalities_explosions/remote_violence",
    "fatalities_riots",
    "fatalities_violence_against_civilians",
    "fatalities_total"
  )
  expect_equal(unique(result_event$variable), events)

  events <- c(
    "fatalities_abduction/forced_disappearance",
    "fatalities_air/drone_strike",
    "fatalities_attack",
    "fatalities_government_regains_territory",
    "fatalities_peaceful_protest",
    "fatalities_total"
  )
  expect_equal(unique(result_sub$variable), events)

  events <- c(
    "fatalities_demonstrations",
    "fatalities_political_violence",
    "fatalities_strategic_developments",
    "fatalities_total"
  )
  expect_equal(unique(result_disorder$variable), events)

  cf <- calc_fatalities_acled(years = 2000, precision_location = 3, precision_time = 3)
  result_all <- cf(x, list(acled))
  expect_equal(nrow(result_all), 7)
  expect_equal(sum(result_all$value), 791 * 2)
  events <- c(
    "fatalities_battles",
    "fatalities_explosions/remote_violence",
    "fatalities_protests",
    "fatalities_riots",
    "fatalities_strategic_developments",
    "fatalities_violence_against_civilians",
    "fatalities_total"
  )
  expect_equal(unique(result_all$variable), events)
})
