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

  expect_snapshot(
    .calc_soilproperties(shp, soilgrids, engine = "extract", stats_soil = c("mean", "median", "sd", "min", "max", "sum", "var")),
  )
  expect_snapshot(
    .calc_soilproperties(shp, soilgrids, engine = "zonal", stats_soil = c("mean", "median", "sd", "min", "max", "sum", "var")),
  )
  expect_snapshot(
    .calc_soilproperties(shp, soilgrids, engine = "exactextract", stats_soil = c("mean", "median", "sd", "min", "max", "sum", "var")),
  )
})
