test_that(".calc_population_count works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  worldpop <- list.files(system.file("res", "worldpop",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  worldpop <- rast(worldpop)

  cpc <- calc_popcount()
  result <- cpc(shp, worldpop)
  cpc <- calc_popcount(stats = c("mean", "median", "sd"))
  result_multi_stat <- cpc(shp, worldpop)
  cpc <- calc_popcount(engine = "zonal")
  result_zonal <- cpc(shp, worldpop)
  cpc <- calc_popcount(engine = "extract")
  result_extract <- cpc(shp, worldpop)
  cpc <- calc_popcount(engine = "exactextract")
  result_exact <- cpc(shp, worldpop)

  expect_equal(
    names(result),
    c("popcount_sum", "year")
  )
  expect_equal(
    names(result_multi_stat),
    c("popcount_mean", "popcount_median", "popcount_sd", "year")
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
    result_zonal$popcount_sum,
    result_extract$popcount_sum,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact$popcount_sum
  )
})
