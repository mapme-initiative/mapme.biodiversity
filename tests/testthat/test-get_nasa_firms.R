# test-get_nasa_firms.R

test_that(".get_nasa_firms works", {
  skip_on_cran()
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

  mapme_options(
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE,
    testing = TRUE
  )

  expect_error(
    get_nasa_firms(years = 2010, instrument = "not-avl"),
    "The selected instrument not-avl is not available."
  )

  gnf <- get_nasa_firms(years = 2010)
  expect_error(
    gnf(),
    "The target years do not intersect with the availability of nasa_firms"
  )

  gnf <- get_nasa_firms(years = 2012)
  expect_equal(
    gnf(aoi),
    "VIIRS_2012.zip"
  )

  gnf <- get_nasa_firms(years = 2012, instrument = "MODIS")
  expect_equal(
    gnf(aoi),
    "MODIS_2012.zip"
  )
})
