test_that("get_soilgrids works", {
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
    verbose = FALSE
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

  expect_error(
    .get_soilgrids(portfolio),
    "For downloading data from soilgrid a valid layer, a valid depth range and a valid statistic have to be specified"
  )
})
