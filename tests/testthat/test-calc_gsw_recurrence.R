test_that("gsw recurrence works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  gswr <- calc_gsw_recurrence(min_recurrence = 10)
  expect_equal(
    gswr(shp, NULL),
    NA
  )

  gsw_recurrence <- list.files(system.file("res", "gsw_recurrence",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_recurrence <- rast(gsw_recurrence)
  rec <- gswr(shp, gsw_recurrence)

  expect_equal(
    rec$gsw_recurrence_area_sum,
    719.590,
    tolerance = 1e-4
  )
})
