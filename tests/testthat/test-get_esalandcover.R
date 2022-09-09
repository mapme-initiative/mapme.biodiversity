test_that(".get_esalandcover works", {
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

  portfolio <- init_portfolio(aoi,
    years = 2015,
    outdir = outdir,
    tmpdir = tmpdir,
    cores = 1,
    add_resources = FALSE,
    verbose = FALSE
  )
  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE
  expect_equal(
    .get_esalandcover(portfolio),
               "W080N20_LC100_v3.0.1_2015.tif")

  # adds test to check for multiple polygons in the same tile
  splitted_aoi <- st_as_sf(st_make_grid(aoi, n = 2))
  portfolio <- init_portfolio(splitted_aoi,
                              years = 2015,
                              outdir = outdir,
                              tmpdir = tmpdir,
                              cores = 1,
                              add_resources = FALSE,
                              verbose = TRUE
  )
  attributes(portfolio)$testing <- TRUE
  expect_equal(
    .get_esalandcover(portfolio),
    "W080N20_LC100_v3.0.1_2015.tif"
  )
})
