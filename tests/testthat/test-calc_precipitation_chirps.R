test_that("precipitation indicator works", {
  shp <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )
  shp <- suppressWarnings(st_cast(shp, to = "POLYGON"))[1, ]

  chirps <- list.files(system.file("res", "chirps",
    package = "mapme.biodiversity"
  ), pattern = "tif$", full.names = TRUE)
  chirps <- rast(chirps)
  expect_warning(cpc <- calc_precipitation_chirps(1980:1982))
  expect_equal(
    cpc(shp, NULL),
    NA
  )
  expect_error(calc_precipitation_chirps(years = 1981:1982, scales_spi = c(1, 12, 48, 60)))
  expect_error(calc_precipitation_chirps(years = 1981:1982, engine = "not-av"))
  cpc <- calc_precipitation_chirps(years = 2000:2010)
  result <- cpc(shp, chirps)
  cpc <- calc_precipitation_chirps(years = 2000:2010, engine = "zonal")
  result_zonal <- cpc(shp, chirps)
  cpc <- calc_precipitation_chirps(years = 2000:2010, engine = "extract")
  result_extract <- cpc(shp, chirps)
  cpc <- calc_precipitation_chirps(years = 2000:2010, engine = "exactextract")
  result_exact <- cpc(shp, chirps)

  expect_equal(
    names(result_zonal[[1]]),
    names(result_extract[[1]])
  )

  expect_equal(
    names(result_zonal[[1]]),
    names(result_exact[[1]])
  )
  expect_equal(
    names(result[[1]]),
    c("dates", "absolute", "anomaly", "spi_3")
  )

  cpc <- calc_precipitation_chirps(years = 2000:2010, scales_spi = c(12, 24), spi_prev_years = 12)
  result2 <- cpc(shp, chirps)
  expect_equal(
    names(result2[[1]]),
    c("dates", "absolute", "anomaly", "spi_12", "spi_24")
  )
  expect_equal(
    result_zonal[[1]]$absolute,
    result_extract[[1]]$absolute,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact[[1]]$absolute
  )
})
