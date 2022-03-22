test_that("precipitation indicator works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  chirps <- list.files(system.file("res", "chirps",
    package = "mapme.biodiversity"
  ), pattern = ".cog$", full.names = TRUE)
  chirps <- rast(chirps)
  attributes(shp)$years <- 1970:1980
  expect_warning(
    .calc_precipitation(shp, chirps),
    "Cannot calculate precipitation statistics for years smaller than 1981"
  )
  attributes(shp)$years <- 1980:1982
  expect_equal(
    .calc_precipitation(shp, NULL),
    tibble(years = NA, months = NA, stat = NA, precipitation = NA)
  )
  attributes(shp)$years <- 1981:1982
  expect_error(
    .calc_precipitation(shp, chirps, stats_precipitation = "not-available"),
    "Argument 'stat' for indicator 'chirps' most be one of 'mean', 'median', 'sd'"
  )
  expect_error(
    .calc_precipitation(shp, chirps, engine = "not-available", stats_precipitation = "mean"),
    "Engine not-available is not an available engine. Please choose one of: zonal, extract, exactextract"
  )
  attributes(shp)$years <- 1981:1983
  expect_snapshot(
    .calc_precipitation(shp, chirps,
      engine = "zonal",
      stats_precipitation = c("mean", "median", "sd")
    )
  )
  expect_snapshot(
    .calc_precipitation(shp, chirps,
      engine = "extract",
      stats_precipitation = c("mean", "median", "sd")
    )
  )
  expect_snapshot(
    .calc_precipitation(shp, chirps,
      engine = "exactextract",
      stats_precipitation = c("mean", "median", "sd")
    )
  )
})
