test_that(".get_gfw_tile_id works", {
  gfw_grid <- make_global_grid()
  expect_equal(.get_gfw_tile_id(gfw_grid[100, ]), "20S_110E")
})

test_that(".get_gfw_treecover works", {
  skip_on_cran()
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  temp_loc <- file.path(tempdir(), "mapme.biodiversity")
  dir.create(temp_loc, showWarnings = FALSE)
  resource_dir <- system.file("res", package = "mapme.biodiversity")
  file.copy(resource_dir, temp_loc, recursive = TRUE)
  outdir <- file.path(tempdir(), "mapme.biodiversity", "res")
  tmpdir <- tempdir()

  mapme_options(
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = TRUE,
    testing = TRUE
  )

  expect_error(get_gfw_treecover(version = "not-available"))
  gt <- get_gfw_treecover(version = "GFC-2020-v1.8")
  expect_equal(gt(aoi), "Hansen_GFC-2020-v1.8_treecover2000_20N_080W.tif")

  # adds test to check for multiple polygons in the same tile
  splitted_aoi <- st_as_sf(st_make_grid(aoi, n = 2))
  expect_equal(gt(splitted_aoi), "Hansen_GFC-2020-v1.8_treecover2000_20N_080W.tif")
})
