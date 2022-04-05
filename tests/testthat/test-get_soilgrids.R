test_that("get_soilgrids works", {
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

  expect_snapshot(
    basename(get_resources(portfolio,
      resources = "soilgrids",
      layers = names(.sg_layers),
      depths = .sg_depths,
      stats = .sg_stats
    ))
  )

  expect_warning(
    get_resources(portfolio,
      resources = "soilgrids",
      layers = "not-available",
      depths = .sg_depths,
      stats = .sg_stats
    )
  )

  expect_warning(
    get_resources(portfolio,
      resources = "soilgrids",
      layers = names(.sg_layers),
      depths = "not-available",
      stats = .sg_stats
    )
  )

  expect_warning(
    get_resources(portfolio,
      resources = "soilgrids",
      layers = names(.sg_layers),
      depths = .sg_depths,
      stats = "not-available"
    )
  )
})