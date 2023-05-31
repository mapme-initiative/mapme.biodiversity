# test-get_ucdp_ged.R

test_that(".get_ucdp_ged works", {
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
    add_resources = FALSE,
    verbose = FALSE
  )
  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE

  expect_error(
    .get_ucdp_ged(portfolio, version = "not-available"),
    "Valid versions for UCDP GED: "
  )

  expect_equal(
    .get_ucdp_ged(portfolio, version = "latest"),
    "ged221-csv.zip"
  )

  expect_equal(
    .get_ucdp_ged(portfolio, version = "21.1"),
    "ged211-csv.zip"
  )
})
