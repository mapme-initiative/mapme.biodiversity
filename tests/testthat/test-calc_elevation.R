test_that("srtm elevation works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nasa_srtm <- list.files(system.file("res", "nasa_srtm",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nasa_srtm <- rast(nasa_srtm)
  attributes(shp)$years <- 2022
  expect_error(
    .calc_elevation(shp, nasa_srtm, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_elevation(shp, nasa_srtm, stats_elevation = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  result <- .calc_elevation(shp, nasa_srtm)
  result_multi_stat <- .calc_elevation(shp, nasa_srtm, stats = c("mean", "median", "sd"))
  result_zonal <- .calc_elevation(shp, nasa_srtm, engine = "zonal")
  result_extract <- .calc_elevation(shp, nasa_srtm, engine = "extract")
  result_exact <- .calc_elevation(shp, nasa_srtm, engine = "exactextract")

  expect_equal(
    names(result),
    c("elevation_mean")
  )
  expect_equal(
    names(result_multi_stat),
    c("elevation_mean", "elevation_median", "elevation_sd")
  )
  expect_equal(
    names(result_zonal),
    names(result_extract)
  )
  expect_equal(
    names(result_zonal),
    names(result_exact)
  )
  expect_equal(
    result_zonal$elevation_mean,
    result_extract$elevation_mean
  )
  expect_snapshot(
    result_exact$elevation_mean
  )
})
