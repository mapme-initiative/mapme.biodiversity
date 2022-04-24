test_that("precipitation indicator works", {
  shp <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )
  shp <- suppressWarnings(st_cast(shp, to = "POLYGON"))[1, ]
  pbapply::pboptions(type = "none")

  chirps <- list.files(system.file("res", "chirps",
    package = "mapme.biodiversity"
  ), pattern = ".cog$", full.names = TRUE)
  chirps <- rast(chirps)
  attributes(shp)$years <- 1970:1980
  attributes(shp)$cores <- 1
  expect_warning(
    .calc_chirpsprec(shp, chirps),
    "Cannot calculate precipitation statistics for years smaller than 1981"
  )
  attributes(shp)$years <- 1980:1982
  expect_equal(
    .calc_chirpsprec(shp, NULL),
    NA
  )
  attributes(shp)$years <- 1981:1982
  expect_error(
    .calc_chirpsprec(shp, chirps, scales_spi = c(1, 12, 48, 60)),
    "Values of 'scales_spi' for SPI calculation must be integers between 0 and 48"
  )
  expect_error(
    .calc_chirpsprec(shp, chirps, engine = "not-available"),
    "Engine not-available is not an available engine. Please choose one of:"
  )
  attributes(shp)$years <- 2000:2010
  expect_snapshot(
    .calc_chirpsprec(shp, chirps)
  )
  expect_snapshot(
    .calc_chirpsprec(shp, chirps, scales_spi = c(12, 24), spi_prev_years = 12)
  )
  expect_snapshot(
    .calc_chirpsprec(shp, chirps, engine = "extract")
  )
  expect_snapshot(
    .calc_chirpsprec(shp, chirps, engine = "exactextract")
  )
  attributes(shp)$years <- 1985:2000
  expect_snapshot(
    .calc_chirpsprec(shp, chirps, scales_spi = c(12, 24), spi_prev_years = 3)
  )
})
