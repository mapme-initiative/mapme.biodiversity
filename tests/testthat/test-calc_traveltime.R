test_that("traveltime works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
                package = "mapme.biodiversity"
    )
  )
  nelson_et_al <- list.files(system.file("res", "nelson_et_al",
                                         package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nelson_et_al <- rast(nelson_et_al)
  attributes(shp)$years <- 2015
  attributes(shp)$cores <- 1
  expect_error(
    .calc_traveltime(shp, nelson_et_al, engine = "not-available"),
    "Engine 'not-available' is not an available engine. Please choose one of:"
  )
  expect_error(
    .calc_traveltime(shp, nelson_et_al, stats_accessibility = "not-available"),
    "Statistic 'not-available' is not supported. Please choose one of:"
  )
  expect_snapshot(
    .calc_traveltime(shp, nelson_et_al)
  )
  expect_snapshot(
    .calc_traveltime(shp, nelson_et_al, engine = "zonal")
  )
  expect_snapshot(
    .calc_traveltime(shp, nelson_et_al, stats = c("mean", "median", "sd", "min", "max", "sum", "var"))
  )
  expect_snapshot(
    .calc_traveltime(shp, nelson_et_al, engine = "extract")
  )
  expect_snapshot(
    .calc_traveltime(shp, nelson_et_al, engine = "exactextract")
  )
})
