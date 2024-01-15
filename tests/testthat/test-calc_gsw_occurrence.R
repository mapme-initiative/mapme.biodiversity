test_that("gsw occurrence works", {
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

  gsw_occurrence <- list.files(system.file("res", "gsw_occurrence",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_occurrence <- rast(gsw_occurrence)
  occ <- .calc_gsw_occurrence(portfolio, gsw_occurrence)

  expect_equal(
    occ$global_surface_water_occurrence_mean,
    19.40947828396224,
    tolerance = 1e-4
  )
})
