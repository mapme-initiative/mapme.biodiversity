test_that("gsw transitions works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  x <- suppressWarnings(st_cast(x, to = "POLYGON")[1, ])
  gswt <- calc_gsw_transitions()
  expect_true(is.null(gswt(x, NULL)))

  gsw_transitions <- list.files(system.file("res", "gsw_transitions",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_transitions <- rast(gsw_transitions)
  transitions <- gswt(x, gsw_transitions)
  expect_silent(.check_single_asset(transitions))

  transitions_expected <- tibble(
    class = c(
      "Permanent", "New Permanent", "Lost Permanent", "Seasonal",
      "New Seasonal", "Seasonal to Permanent", "Permanent to Seasonal",
      "Ephemeral Permanent", "Ephemeral Seasonal"
    ),
    area = c(
      199.40, 388.84, 0.38,
      1.90, 83.90, 28.859,
      1.06, 0.60, 21.62
    )
  )
  expect_equal(transitions$value, transitions_expected$area, tolerance = 1e-4)
})
