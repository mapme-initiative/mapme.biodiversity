test_that(".get_fritz_et_al works", {
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

  portfolio <- init_portfolio(aoi,
    years = 2000:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    add_resources = FALSE,
    verbose = TRUE
  )
  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE
  expect_equal(
    .get_fritz_et_al(portfolio, res_drivers = 100, rundir = file.path(outdir, "fritz_et_al")),
    "Deforestation_Drivers_100m_IIASA.zip"
  )

  expect_equal(
    .get_fritz_et_al(portfolio, res_drivers = 1000, rundir = file.path(outdir, "fritz_et_al")),
    "Deforestation_drivers_1km_IIASA_.zip"
  )

  expect_error(
    .get_fritz_et_al(portfolio, res_drivers = 200, rundir = file.path(outdir, "fritz_et_al")),
    "Fritz et al. resource is available only at resolutions 100 and 1.000"
  )
})
