test_that("gsw seasonality works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  portfolio <- init_portfolio(
    shp,
    years = 2024,
    tmpdir =  "~/R/mapme.biodiversity/inst/res/gsw_seasonality",
    verbose = TRUE
  )

  gsw_seasonality <- list.files(system.file("res", "gsw_seasonality",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_seasonality <- rast(gsw_seasonality)
  season <- .calc_gsw_seasonality(portfolio, gsw_seasonality)

  expect_equal(
    season$global_surface_water_seasonality_mean,
    2.289487662369018,
    tolerance = 1e-4
  )
})
