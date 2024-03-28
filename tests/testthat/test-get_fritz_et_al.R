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

  mapme_options(
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE,
    testing = TRUE
  )

  gf <- get_fritz_et_al(resolution = 100)
  expect_equal(
    gf(aoi),
    "Deforestation_Drivers_100m_IIASA.zip"
  )

  gf <- get_fritz_et_al(resolution = 1000)
  expect_equal(
    gf(aoi),
    "Deforestation_drivers_1km_IIASA_.zip"
  )

  expect_error(
    get_fritz_et_al(resolution = 200),
    "Fritz et al. resource is available only at resolutions 100 and 1.000"
  )
})
