test_that("get_iucn works", {
  iucn_dir <- system.file("res", "iucn", package = "mapme.biodiversity")
  iucn_files <- list.files(iucn_dir, pattern = "_SR_", full.names = TRUE)

  msg <- "Expecting paths to point towards existing GTiff files."
  expect_error(get_iucn(NULL), msg)
  expect_error(get_iucn("does-not-exist.tif"), msg)
  gpkg <- system.file("extdata", "burundi.gpkg", package = "mapme.biodiversity")
  expect_error(get_iucn(gpkg), msg)
  other_tif <- system.file("extdata", "burundi_worldpop.tif", package = "mapme.biodiversity")
  msg <- "Filenames do match expected schema for IUCN species richness rasters."
  expect_error(get_iucn(c(iucn_files, other_tif)), msg)
  fps <- get_iucn(iucn_files)()
  expect_silent(.check_footprints(fps))
})
