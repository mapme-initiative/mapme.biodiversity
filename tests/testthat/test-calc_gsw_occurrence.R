test_that("gsw occurrence works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  x <- suppressWarnings(st_cast(x, to = "POLYGON")[1, ])
  gswo <- calc_gsw_occurrence(min_occurrence = 10)
  expect_true(is.null(gswo(x, NULL)))

  gsw_occurrence <- list.files(system.file("res", "gsw_occurrence",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_occurrence <- rast(gsw_occurrence)
  occ <- gswo(x, gsw_occurrence)
  expect_equal(occ$value, 694.159, tolerance = 1e-4)
})
