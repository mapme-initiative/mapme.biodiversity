test_that("gsw recurrence works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  tmpdir <- tempdir()
  portfolio <- init_portfolio(
    shp,
    years = 2024,
    tmpdir = tmpdir,
    verbose = TRUE
  )

  gsw_recurrence <- list.files(system.file("res", "gsw_recurrence",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_recurrence <- rast(gsw_recurrence)
  rec <- .calc_gsw_recurrence(portfolio, gsw_recurrence)

  expect_equal(
    rec$global_surface_water_recurrence_mean,
    22.49229803467092,
    tolerance = 1e-4
  )
})

