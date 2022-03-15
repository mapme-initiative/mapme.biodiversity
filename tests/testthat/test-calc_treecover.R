test_that("treecover works", {
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
  portfolio <- init_portfolio(aoi,
    years = 1990:1995,
    cores = 1,
    outdir = outdir,
    tmpdir = tmpdir
  )
  portfolio <- get_resources(portfolio,
    resources = c("treecover", "lossyear", "greenhouse")
  )
  resources <- attributes(portfolio)$resources

  expect_warning(
    calc_indicators(portfolio, "treecover"),
    "Cannot calculate treecover statistics for years smaller than 2000"
  )

  attributes(portfolio)$years <- 1999:2005
  expect_snapshot(calc_indicators(portfolio, "treecover")$treecover[[1]])
  attributes(portfolio)$years <- 2000:2005

  expect_error(
    calc_indicators(portfolio, "treecover", min_cover = "10"),
    "must be a numeric value between 0 and 100"
  )

  expect_error(
    calc_indicators(portfolio, "treecover", min_cover = 200),
    "must be a numeric value between 0 and 100"
  )

  expect_snapshot(
    calc_indicators(portfolio, "treecover", min_cover = 50.2)$treecover[[1]]
  )

  expect_error(
    calc_indicators(portfolio, "treecover", min_size = -10),
    "must be anumeric value greater 0"
  )

  expect_snapshot(
    calc_indicators(portfolio, "treecover", min_size = 1000, min_cover = 100)$treecover[[1]]
  )

  attributes(portfolio)$cores <- 2
  stats_treecover <- calc_indicators(portfolio, "treecover", min_size = 10, min_cover = 30)$treecover[[1]]$treecover
  stats_treeloss <- calc_indicators(portfolio, "treeloss", min_size = 10, min_cover = 30)$treeloss[[1]]$treecover
  expect_equal(stats_treecover, stats_treecover)
})
