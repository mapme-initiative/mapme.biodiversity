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
  attributes(shp)$years <- 2000:2022
  expect_error(
    .calc_population_count(shp, worldpop, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_population_count(shp, worldpop, stats_popcount = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )

  result <- .calc_population_count(shp, worldpop)
  result_multi_stat <- .calc_population_count(shp, worldpop, stats_popcount = c("mean", "median", "sd"))
  result_zonal <- .calc_population_count(shp, worldpop, engine = "zonal")
  result_extract <- .calc_population_count(shp, worldpop, engine = "extract")
  result_exact <- .calc_population_count(shp, worldpop, engine = "exactextract")

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
