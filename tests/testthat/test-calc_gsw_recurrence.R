test_that("gsw recurrence works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  x <- suppressWarnings(st_cast(x, to = "POLYGON")[1, ])
  gswr <- calc_gsw_recurrence(min_recurrence = 10)
  expect_true(is.null(gswr(x, NULL)))

  gsw_recurrence <- list.files(system.file("res", "gsw_recurrence",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_recurrence <- rast(gsw_recurrence)
  rec <- gswr(x, gsw_recurrence)
  expect_equal(rec$value, 719.590, tolerance = 1e-4)
})
