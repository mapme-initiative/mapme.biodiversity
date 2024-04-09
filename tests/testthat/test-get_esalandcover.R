test_that(".get_esalandcover works", {
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
    verbose = FALSE,
    testing = TRUE
  )
  # Add testing attribute in order to skip downloads
  gel <- get_esalandcover(years = 2015)
  expect_equal(
    gel(aoi),
    "W080N20_LC100_v3.0.1_2015.tif"
  )

  # adds test to check for multiple polygons in the same tile
  splitted_aoi <- st_as_sf(st_make_grid(aoi, n = 2))
  expect_equal(
    gel(splitted_aoi),
    "W080N20_LC100_v3.0.1_2015.tif"
  )
})
