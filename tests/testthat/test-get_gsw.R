test_that(".get_gsw works", {
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

  portfolio <- init_portfolio(aoi,
    years = 2000:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = TRUE
  )
  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE

  expect_error(
    .get_gsw(portfolio, statistic = "not-available")
  )

  expect_equal(
    basename(.get_gsw(portfolio, statistic = "occurrence")),
    "occurrence_80W_20Nv1_4_2021.tif"
  )

  # adds test to check for multiple polygons in the same tile
  splitted_aoi <- st_as_sf(st_make_grid(aoi, n = 2))
  portfolio <- init_portfolio(splitted_aoi,
    years = 2000:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = TRUE
  )
  attributes(portfolio)$testing <- TRUE
  expect_equal(
    basename(.get_gsw(portfolio, statistic = "occurrence")),
    "occurrence_80W_20Nv1_4_2021.tif"
  )
})
