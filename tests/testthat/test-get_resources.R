test_that("get_resources works", {
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
    years = 2000:2020,
    cores = 1,
    outdir = outdir,
    tmpdir = tmpdir
  )

  expect_message(
    get_resources(portfolio,
      resources = c("treecover", "lossyear", "greenhouse")
    ),
    "Setting to default value of"
  )

  expect_error(
    get_resources(portfolio,
      resources = c("not_available")
    ),
    "requested resource is not supported"
  )

  expect_message(
    get_resources(portfolio,
      resources = c("treecover", "lossyear", "greenhouse"),
      vers_treecover = "GFC-2020-v1.8",
      vers_lossyear = "GFC-2020-v1.8"
    ),
    "Starting process to download resource"
  )

  expect_warning(
    get_resources(portfolio,
      resources = c("lossyear"),
      vers_lossyear = "not_available"
    ),
    "Download for resource lossyear failed. Returning unmodified portfolio object."
  )

  expect_message(
    get_resources(portfolio,
      resources = c("treecover", "lossyear", "greenhouse")
    ),
    "Skipping existing files in output directory"
  )

  portfolio2 <- get_resources(portfolio,
    resources = c("treecover", "lossyear", "greenhouse")
  )

  expect_message(
    get_resources(portfolio2,
      resources = c("treecover", "lossyear", "greenhouse")
    ),
    "The following requested resources are already available: treecover, lossyear, greenhouse."
  )
})
