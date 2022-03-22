test_that("precipitation indicator works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  chirps <- list.files(system.file("res", "chirps",
    package = "mapme.biodiversity"
  ), pattern = ".cog$", full.names = TRUE)
  chirps <- rast(chirps)
  expect_snapshot(
    .calc_avgperc(shp, NULL)
  )
  expect_snapshot(
    .calc_avgperc(shp, chirps, engine = "zonal")
  )
  expect_snapshot(
    .calc_avgperc(shp, chirps, engine = "extract")
  )
  expect_snapshot(
    .calc_avgperc(shp, chirps, engine = "exactextract")
  )
  expect_error(
    .calc_avgperc(shp, chirps, engine = "not-available")
  )
  chirps[] <- NA
  expect_snapshot(
    .calc_avgperc(shp, chirps, engine = "zonal")
  )
})
