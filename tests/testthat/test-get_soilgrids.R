test_that("get_soilgrids works", {
  skip_on_cran()
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- suppressWarnings(st_cast(x, to = "POLYGON")[1, ])

  outdir <- file.path(tempdir(), "mapme-data")
  dir.create(outdir, showWarnings = FALSE)
  .copy_resource_dir(outdir)

  mapme_options(
    outdir = outdir,
    verbose = FALSE,
    testing = TRUE
  )

  gsg <- get_soilgrids(layers = names(.sg_layers), depths = .sg_depths, stats = .sg_stats)
  expect_snapshot(
    gsg(x)
  )

  expect_error(
    get_soilgrids(
      layers = "not-available",
      depths = .sg_depths,
      stats = .sg_stats
    )
  )

  expect_error(
    get_soilgrids(
      layers = names(.sg_layers),
      depths = "not-available",
      stats = .sg_stats
    )
  )

  expect_error(
    get_soilgrids(
      layers = names(.sg_layers),
      depths = .sg_depths,
      stats = "not-available"
    )
  )

  expect_error(
    get_soilgrids(),
    "For downloading data from soilgrid a valid layer, a valid depth range and a valid statistic have to be specified"
  )
})
