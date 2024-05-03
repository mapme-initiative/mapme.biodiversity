test_that("gsw change works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  x <- suppressWarnings(st_cast(x, to = "POLYGON")[1, ])
  gswc <- calc_gsw_change()
  expect_true(is.null(gswc(x, NULL)))

  gsw_change <- list.files(system.file("res", "gsw_change",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_change <- rast(gsw_change)
  chg <- gswc(x, gsw_change)
  expect_silent(.check_single_asset(chg))
  expect_equal(chg$value, 100, tolerance = 1e-4)
})
