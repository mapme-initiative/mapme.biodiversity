test_that(".calc_fatalities_ucpd works", {
  x <- read_sf(
    system.file("extdata", "burundi.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_ucdp_ged(version = "22.1"))
  ucdp_ged <- prep_resources(x)[["ucdp_ged"]][[1]]

  expect_error(
    calc_fatalities_ucdp(years = 1980),
    "The target years do not intersect with the availability of ucdp_ged."
  )
  expect_error(
    calc_fatalities_ucdp(precision_location = 8),
    "Argument precision_location must be a single numeric between 1 and 7."
  )
  expect_error(
    calc_fatalities_ucdp(precision_time = 6),
    "Argument precision_time must be a single numeric between 1 and 5."
  )

  cf <- calc_fatalities_ucdp(years = 1991:1992)
  result_default <- cf(x, list(ucdp_ged))
  cf <- calc_fatalities_ucdp(years = 1991:1992, precision_location = 7, precision_time = 5)
  result_all <- cf(x, list(ucdp_ged))

  conf_types <- c("state_based_conflict", "non_state_conflict", "one_sided_violence")
  death_types <- c("deaths_civilians", "deaths_unknown", "deaths_total")
  vars <- paste("fatalities", rep(conf_types, each = length(death_types)), death_types, sep = "_")

  expect_silent(.check_single_asset(result_default))
  expect_equal(unique(result_default$variable), vars[7:9])
  expect_equal(nrow(result_default), 3)
  expect_equal(sum(result_default$value), 40)

  expect_equal(unique(result_all$variable), vars[-c(4:6)])
  expect_equal(nrow(result_all), 9)
  expect_equal(sum(result_all$value), 592)
})
