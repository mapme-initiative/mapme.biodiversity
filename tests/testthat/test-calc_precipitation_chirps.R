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
  attributes(shp)$years <- 1970:1980
  expect_warning(
    .calc_precipitation_chirps(shp, chirps),
    "Cannot calculate precipitation statistics for years smaller than 1981"
  )
  attributes(shp)$years <- 1980:1982
  expect_equal(
    .calc_precipitation_chirps(shp, NULL),
    NA
  )
  attributes(shp)$years <- 1981:1982
  expect_error(
    .calc_precipitation_chirps(shp, chirps, scales_spi = c(1, 12, 48, 60)),
    "Values of 'scales_spi' for SPI calculation must be integers between 0 and 48"
  )
  expect_error(
    .calc_precipitation_chirps(shp, chirps, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  attributes(shp)$years <- 2000:2010
  result <- .calc_precipitation_chirps(shp, chirps)
  result_zonal <- .calc_precipitation_chirps(shp, chirps, engine = "zonal")
  result_extract <- .calc_precipitation_chirps(shp, chirps, engine = "extract")
  result_exact <- .calc_precipitation_chirps(shp, chirps, engine = "exactextract")

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

  result2 <- .calc_precipitation_chirps(shp, chirps, scales_spi = c(12, 24), spi_prev_years = 12)
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
