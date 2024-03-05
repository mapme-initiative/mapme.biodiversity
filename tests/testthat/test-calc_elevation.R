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
  ce <- calc_elevation()
  result <- ce(shp, nasa_srtm)
  ce <- calc_elevation(stats = c("mean", "median", "sd"))
  result_multi_stat <- ce(shp, nasa_srtm)
  ce <- calc_elevation(engine = "zonal")
  result_zonal <- ce(shp, nasa_srtm)
  ce <- calc_elevation(engine = "extract")
  result_extract <- ce(shp, nasa_srtm)
  ce <- calc_elevation(engine = "exactextract")
  result_exact <- ce(shp, nasa_srtm)

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
