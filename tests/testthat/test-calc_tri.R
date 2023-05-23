test_that("terrain ruggedness index works", {
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
    .calc_tri(shp, nasa_srtm, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_tri(shp, nasa_srtm, stats_tri = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )

  result <- .calc_tri(shp, nasa_srtm)
  result_multi_stat <- .calc_tri(shp, nasa_srtm, stats = c("mean", "median", "sd"))
  result_zonal <- .calc_tri(shp, nasa_srtm, engine = "zonal")
  result_extract <- .calc_tri(shp, nasa_srtm, engine = "extract")
  result_exact <- .calc_tri(shp, nasa_srtm, engine = "exactextract")

  expect_equal(
    names(result),
    c("tri_mean")
  )
  expect_equal(
    names(result_multi_stat),
    c("tri_mean", "tri_median", "tri_sd")
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
    result_zonal$tri_mean,
    result_extract$tri_mean,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact$tri_mean
  )
})
