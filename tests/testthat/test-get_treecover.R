test_that(".get_treecover works", {
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  outdir <- system.file("res",
    package = "mapme.biodiversity"
  )
  tmpdir <- system.file("tmp",
    package = "mapme.biodiversity"
  )

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

  expect_warning(
    get_resources(portfolio, resources = "treecover2000", vers_treecover = "not-available"),
    "Download for resource treecover2000 failed. Returning unmodified portfolio object."
  )

  expect_equal(
    basename(get_resources(portfolio, resources = "treecover2000")),
    "Hansen_GFC-2020-v1.8_treecover2000_20N_080W.tif"
  )
})
