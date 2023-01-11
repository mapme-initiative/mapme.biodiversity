# test-get_nasa_firms.R

test_that(".get_nasa_firms works", {
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
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
                              years = 2010,
                              outdir = outdir,
                              tmpdir = tmpdir,
                              cores = 1,
                              add_resources = FALSE,
                              verbose = FALSE
  )
  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE

  expect_error(
    .get_nasa_firms(portfolio, instrument = "VIIRS_NOAA"),
    "The selected instrument VIIRS_NOAA is not available."
  )

  expect_error(
    .get_nasa_firms(portfolio),
    "The target years do not intersect with the availability of nasa_firms"
  )

  attributes(portfolio)$years = 2012
  expect_equal(
    .get_nasa_firms(portfolio),
    "VIIRS_2012.zip"
  )

  expect_equal(
    .get_nasa_firms(portfolio, instrument = "MODIS"),
    "MODIS_2012.zip"
  )
})
