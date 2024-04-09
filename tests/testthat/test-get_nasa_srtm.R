test_that(".get_nasa_srtm works", {
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
    verbose = TRUE,
    testing = TRUE
  )

  gns <- get_nasa_srtm()
  expect_equal(gns(aoi), "NASADEM_HGT_n18w072.tif")
  splitted_aoi <- st_as_sf(st_make_grid(aoi, n = 2))
  expect_equal(gns(splitted_aoi), "NASADEM_HGT_n18w072.tif")
})
