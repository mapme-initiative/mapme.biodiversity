test_that(".get_nelson_et_al works", {
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
                              cores = 1,
                              add_resources = FALSE,
                              verbose = TRUE
  )
  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE
  expect_equal(
    .get_nelson_et_al(portfolio, range_traveltime = "20k_50k"),
    "traveltime-20k_50k.tif"
  )
})
