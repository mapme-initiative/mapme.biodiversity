test_that("precipitation indicator works", {
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- suppressWarnings(st_cast(x, to = "POLYGON"))[1, ]

  chirps <- list.files(system.file("res", "chirps",
    package = "mapme.biodiversity"
  ), pattern = "tif$", full.names = TRUE)
  chirps <- rast(chirps)
  expect_message(cpc <- calc_precipitation_chirps(1980:1982))
  expect_error(calc_precipitation_chirps(years = 1970:1971))
  expect_error(calc_precipitation_chirps(years = 1981:1982, engine = "not-av"))
  expect_true(is.null(cpc(x, NULL)))
  cpc <- calc_precipitation_chirps(years = 2010)
  result <- cpc(x, chirps)
  cpc <- calc_precipitation_chirps(years = 2010, engine = "zonal")
  result_zonal <- cpc(x, chirps)
  cpc <- calc_precipitation_chirps(years = 2010, engine = "extract")
  result_extract <- cpc(x, chirps)
  cpc <- calc_precipitation_chirps(years = 2010, engine = "exactextract")
  result_exact <- cpc(x, chirps)

  expect_silent(.check_single_asset(result[[1]]))
  expect_silent(.check_single_asset(result_zonal[[1]]))
  expect_silent(.check_single_asset(result_extract[[1]]))
  expect_silent(.check_single_asset(result_exact[[1]]))

  expect_equal(unique(result[[1]]$variable), "precipitation")
  expect_equal(result_zonal[[1]]$value, result_extract[[1]]$value,
    tolerance = 1e-4
  )
  expect_snapshot(result[[1]]$value)
})
