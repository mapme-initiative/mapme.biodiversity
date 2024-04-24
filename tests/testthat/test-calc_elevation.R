test_that("srtm elevation works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nasa_srtm <- list.files(system.file("res", "nasa_srtm",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nasa_srtm <- rast(nasa_srtm)
  ce <- calc_elevation()
  result <- ce(x, nasa_srtm)
  ce <- calc_elevation(stats = c("mean", "median", "sd"))
  result_multi_stat <- ce(x, nasa_srtm)
  ce <- calc_elevation(engine = "zonal")
  result_zonal <- ce(x, nasa_srtm)
  ce <- calc_elevation(engine = "extract")
  result_extract <- ce(x, nasa_srtm)
  ce <- calc_elevation(engine = "exactextract")
  result_exact <- ce(x, nasa_srtm)

  expect_silent(.check_single_asset(result))
  expect_equal(
    unique(result_multi_stat$variable),
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
    result_zonal$value,
    result_extract$value
  )
  expect_snapshot(
    result_exact$value
  )
})
