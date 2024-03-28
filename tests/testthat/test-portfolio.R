test_that("portoflio I/O works as expected", {
  skip()
  portfolio <- init_portfolio(aoi,
    years = 2000:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE,
  ) %>%
    get_resources(c("gfw_treecover", "gfw_lossyear"),
      vers_lossyear = "GFC-2020-v1.8", vers_treecover = "GFC-2020-v1.8"
    )

  tmpfile <- file.path(file.path(tempdir(), "portfolio_out.gpkg"))
  portfolio <- calc_indicators(portfolio, "treecover_area", min_size = 1, min_cover = 30)

  expect_invisible(
    write_portfolio(portfolio, tmpfile, quiet = TRUE)
  )

  expect_silent(
    portfolio2 <- read_portfolio(tmpfile)
  )
  expect_equal(
    names(portfolio2),
    c("WDPAID", "NAME", "DESIG_ENG", "ISO3", "assetid", "treecover_area", "geom")
  )

  file.remove(tmpfile)
})
