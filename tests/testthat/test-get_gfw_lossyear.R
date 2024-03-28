test_that(".get_gfw_lossyear works", {
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

  gl <- get_gfw_lossyear(version = "GFC-2020-v1.8")
  expect_equal(gl(aoi), "Hansen_GFC-2020-v1.8_lossyear_20N_080W.tif")
  splitted_aoi <- st_as_sf(st_make_grid(aoi, n = 2))
  expect_equal(
    gl(splitted_aoi),
    "Hansen_GFC-2020-v1.8_lossyear_20N_080W.tif"
  )
})
