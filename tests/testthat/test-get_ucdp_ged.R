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

  mapme_options(
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE,
    testing = TRUE
  )

  expect_error(
    get_ucdp_ged(version = "not-available"),
    "Valid versions for UCDP GED: "
  )

  guc <- get_ucdp_ged(version = "latest")
  expect_equal(
    guc(aoi),
    "ged231-csv.gpkg"
  )

  guc <- get_ucdp_ged(version = "21.1")
  expect_equal(
    guc(),
    "ged211-csv.gpkg"
  )

  skip_on_cran()
  expect_equal(
    .ucdp_versions(),
    c("5.0", "17.1", "17.2", "18.1", "19.1", "652.1601.1911", "20.1", "21.1", "22.1", "23.1")
  )
})
