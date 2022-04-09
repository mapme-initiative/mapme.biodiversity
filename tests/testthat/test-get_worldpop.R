test_that(".get_worldpop works", {
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
    years = 2001,
    outdir = outdir,
    tmpdir = tmpdir,
    cores = 1,
    add_resources = FALSE,
    verbose = TRUE
  )
  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE

  expect_equal(
    basename(get_resources(portfolio, "worldpop")),
    "ppp_2001_1km_Aggregated.tif"
  )

  expect_warning(
    .get_worldpop_url(1999),
    "Population count not available for target year 1999"
  )
})
