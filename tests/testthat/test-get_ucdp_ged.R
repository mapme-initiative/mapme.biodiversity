# test-get_ucdp_ged.R
test_that(".get_ucdp_ged works", {
  skip_on_cran()
  aoi <- read_sf(
    system.file("extdata", "burundi.gpkg",
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
    "ged231-csv.gpkg"
  )

  expect_equal(
    .get_ucdp_ged(portfolio, version = "21.1"),
    "ged211-csv.gpkg"
  )

  skip_on_cran()
  expect_equal(
    .ucdp_versions(),
    c("5.0", "17.1", "17.2", "18.1", "19.1", "652.1601.1911", "20.1", "21.1", "22.1", "23.1")
  )
})
