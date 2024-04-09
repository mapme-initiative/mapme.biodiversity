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

  csp <- calc_soilproperties()
  expect_equal(
    csp(shp, NULL),
    NA
  )

  csp <- calc_soilproperties()
  result <- csp(shp, soilgrids)
  csp <- calc_soilproperties(engine = "extract", stats = c("mean", "median", "sd"))
  result_multi_stat <- csp(shp, soilgrids)
  csp <- calc_soilproperties(engine = "zonal")
  result_zonal <- csp(shp, soilgrids)
  csp <- calc_soilproperties(engine = "extract")
  result_extract <- csp(shp, soilgrids)
  csp <- calc_soilproperties(engine = "exactextract")
  result_exact <- csp(shp, soilgrids)

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
