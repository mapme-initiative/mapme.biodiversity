test_that("gsw recurrence works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  tmpdir <- tempdir()
  portfolio <- init_portfolio(
    shp,
    years = 2024,
    tmpdir = tmpdir,
    verbose = TRUE
  )

  expect_equal(
    .calc_gsw_recurrence(portfolio, NULL),
    NA
  )

  gsw_recurrence <- list.files(system.file("res", "gsw_recurrence",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_recurrence <- rast(gsw_recurrence)
  rec <- .calc_gsw_recurrence(portfolio, gsw_recurrence, min_recurrence = 10)

  expect_error(
    .calc_gsw_recurrence(portfolio, gsw_recurrence, min_occurrence = NULL)
  )

  expect_equal(
    rec$gsw_recurrence_area_sum,
    719.590,
    tolerance = 1e-4
  )
})

