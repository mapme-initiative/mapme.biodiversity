test_that("gsw occurrence works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  gswo <- calc_gsw_occurrence(min_occurrence = 10)
  expect_equal(
    gswo(shp, NULL),
    NA
  )

  gsw_occurrence <- list.files(system.file("res", "gsw_occurrence",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_occurrence <- rast(gsw_occurrence)
  occ <- gswo(shp, gsw_occurrence)

  expect_equal(
    occ$gsw_occurrence_area_sum,
    694.159,
    tolerance = 1e-4
  )
})
