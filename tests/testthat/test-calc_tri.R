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

  ctri <- calc_tri()
  result <- ctri(shp, nasa_srtm)
  ctri <- calc_tri(stats = c("mean", "median", "sd"))
  result_multi_stat <- ctri(shp, nasa_srtm)
  ctri <- calc_tri(engine = "zonal")
  result_zonal <- ctri(shp, nasa_srtm)
  ctri <- calc_tri(engine = "extract")
  result_extract <- ctri(shp, nasa_srtm)
  ctri <- calc_tri(engine = "exactextract")
  result_exact <- ctri(shp, nasa_srtm)

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
