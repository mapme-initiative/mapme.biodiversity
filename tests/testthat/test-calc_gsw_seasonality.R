test_that("gsw seasonality works", {
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
    tmpdir =  tmpdir,
    verbose = TRUE
  )

  expect_equal(
    .calc_gsw_seasonality(portfolio, NULL),
    NA
  )

  gsw_seasonality <- list.files(system.file("res", "gsw_seasonality",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_seasonality <- rast(gsw_seasonality)
  season <- .calc_gsw_seasonality(portfolio, gsw_seasonality)

  expect_equal(
    season$month,
    seq_len(12)
  )

  expect_equal(
    season$area,
    c(22.461, 17.968, 23.070, 19.872, 2.284, 0.152, 0, 0, 0, 0, 0, 583.375),
    tolerance = 1e-3
  )
})
