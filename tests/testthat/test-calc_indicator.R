test_that("multiplication works", {
  skip_on_os(os = "windows")
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

  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON"))[1, ]
  portfolio <- init_portfolio(aoi,
    years = 2000:2005,
    cores = 1,
    outdir = outdir,
    tmpdir = tmpdir
  )

  portfolio <- get_resources(portfolio,
    resources = c("treecover2000", "lossyear")
  )

  expect_message(
    calc_indicators(portfolio,
      indicators = "treecover"
    ),
    "was not specified. Setting to default value"
  )

  stat <- calc_indicators(portfolio,
    indicators = "treecover",
    min_size = 5,
    min_cover = 30
  )$treecover[[1]]
  expect_snapshot(stat)

  portfolio <- init_portfolio(aoi,
    years = 2000:2005,
    cores = 2,
    outdir = outdir,
    tmpdir = tmpdir
  )
  portfolio <- get_resources(portfolio,
    resources = c("treecover2000", "lossyear")
  )

  stat <- calc_indicators(portfolio,
    indicators = "treecover",
    min_size = 5,
    min_cover = 30
  )$treecover[[1]]
  expect_snapshot(
    stat
  )
})
