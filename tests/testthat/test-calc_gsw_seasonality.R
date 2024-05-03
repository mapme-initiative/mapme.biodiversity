test_that("gsw seasonality works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  x <- suppressWarnings(st_cast(x, to = "POLYGON")[1, ])
  gsws <- calc_gsw_seasonality()
  expect_true(is.null(gsws(x, NULL)))

  gsw_seasonality <- list.files(system.file("res", "gsw_seasonality",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_seasonality <- rast(gsw_seasonality)
  season <- gsws(x, gsw_seasonality)
  expect_equal(season$value,
    c(2514.759, 22.461, 18.045, 23.831, 19.872, 2.512, 0.152, 0, 0, 0, 0, 0, 617.104),
    tolerance = 1e-3
  )
})
