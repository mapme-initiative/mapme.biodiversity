test_that("gsw seasonality works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  gsws <- calc_gsw_seasonality()
  expect_equal(
    gsws(shp, NULL),
    NA
  )

  gsw_seasonality <- list.files(system.file("res", "gsw_seasonality",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_seasonality <- rast(gsw_seasonality)
  season <- gsws(shp, gsw_seasonality)

  expect_equal(
    season$month,
    seq(0, 12)
  )

  expect_equal(
    season$area,
    c(2514.759, 22.461, 18.045, 23.831, 19.872, 2.512, 0.152, 0, 0, 0, 0, 0, 617.104),
    tolerance = 1e-3
  )
})
