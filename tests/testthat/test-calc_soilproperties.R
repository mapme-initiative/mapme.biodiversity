test_that("soilpoperties works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  shp <- suppressWarnings(st_cast(shp, to = "POLYGON"))[1, ]

  soilgrids <- list.files(system.file("res", "soilgrids",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)

  soilgrids <- rast(soilgrids)

  expect_equal(
    .calc_soilproperties(shp, NULL),
    NA
  )

  expect_error(
    .calc_soilproperties(shp, soilgrids, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of: zonal, extract, exactextract"
  )
  expect_error(
    .calc_soilproperties(shp, soilgrids, stats_soil = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )

  result <- .calc_soilproperties(shp, soilgrids)
  result_multi_stat <- .calc_soilproperties(shp, soilgrids, engine = "extract", stats_soil = c("mean", "median", "sd"))
  result_zonal <- .calc_soilproperties(shp, soilgrids, engine = "zonal")
  result_extract <- .calc_soilproperties(shp, soilgrids, engine = "extract")
  result_exact <- .calc_soilproperties(shp, soilgrids, engine = "exactextract")

  expect_equal(
    names(result),
    c("layer", "depth", "stat", "mean")
  )
  expect_equal(
    names(result_multi_stat),
    c("layer", "depth", "stat", "mean", "median", "sd")
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
    result_zonal$mean,
    result_extract$mean,
    tolerance = 1e-4
  )
  expect_snapshot(
    result_exact$mean
  )
})
