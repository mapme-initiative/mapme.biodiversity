test_that("gsw change works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  portfolio <- init_portfolio(
    shp,
    years = 2024,
    tmpdir =  "~/R/mapme.biodiversity/inst/res/gsw_change",
    verbose = TRUE
  )

  gsw_change <- list.files(system.file("res", "gsw_change",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_change <- rast(gsw_change)
  chg <- .calc_gsw_change(portfolio, gsw_change)

  expect_equal(
    chg$global_surface_water_change_mean,
    100,
    tolerance = 1e-4
  )
})
