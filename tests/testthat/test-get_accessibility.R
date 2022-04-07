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
    basename(get_resources(portfolio, "traveltime")),
    "traveltime-20k_50k.tif"
  )

  expect_warning(
    get_resources(portfolio, "traveltime", range_traveltime = "not-available"),
    "Download for resource traveltime failed. Returning unmodified portfolio object."
  )

  expect_warning(
    get_resources(portfolio, "traveltime", range_traveltime = c("not-available", "not-available2")),
    "Download for resource traveltime failed. Returning unmodified portfolio object."
  )

  expect_equal(
    basename(get_resources(portfolio, "traveltime", range_traveltime = c("20k_50k", "not-available"))),
    "traveltime-20k_50k.tif"
  )
})
