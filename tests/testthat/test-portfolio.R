
test_that("init_portfolio works", {
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )


  outdir <- system.file("res",
    package = "mapme.biodiversity"
  )
  tmpdir <- system.file("tmp",
    package = "mapme.biodiversity"
  )
  expect_error(
    init_portfolio(aoi,
      years = 1980:2020,
      outdir = outdir,
      tmpdir = outdir,
      cores = 2,
      verbose = TRUE
    ),
    "outdir and tmpdir need to point to different directories."
  )


  atts <- attributes(init_portfolio(aoi,
    years = 1980:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    cores = 2,
    verbose = TRUE
  ))
  atts$outdir <- gsub(".*/mapme", "", atts$outdir)
  atts$tmpdir <- gsub(".*/mapme", "", atts$tmpdir)

  expect_snapshot(
    atts
  )

  portfolio <- init_portfolio(aoi,
    years = 1980:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    cores = 2,
    verbose = TRUE
  )

  expect_message(
    init_portfolio(portfolio,
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      cores = 2,
      verbose = TRUE
    ),
    "'.assetid'. Overwritting its values with a unique identifier."
  )

  aoi2 <- st_transform(aoi, st_crs(3857))
  expect_message(
    init_portfolio(aoi2,
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      cores = 2,
      verbose = TRUE
    ),
    "CRS of x is not EPSG:4326. Attempting to transform."
  )

  expect_error(
    init_portfolio(st_cast(aoi, "MULTILINESTRING"),
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      cores = 2,
      verbose = TRUE
    ),
    "Some assests are not of type POLYGON or MULTIPOLYGON."
  )

  expect_error(
    init_portfolio(dplyr::filter(aoi, WDPAID == 1),
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      cores = 2,
      verbose = TRUE
    ),
    "x must contain at least one asset."
  )
})
