test_that("gsw change works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  gswc <- calc_gsw_change()
  expect_equal(
    gswc(shp, NULL),
    NA
  )

  gsw_change <- list.files(system.file("res", "gsw_change",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_change <- rast(gsw_change)
  chg <- gswc(shp, gsw_change)

  expect_equal(
    chg$global_surface_water_change_mean,
    100,
    tolerance = 1e-4
  )
})
