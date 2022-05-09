test_that("get_resources works", {
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
    cores = 1,
    outdir = outdir,
    tmpdir = tmpdir,
    add_resources = FALSE,
    verbose = FALSE
  )

  expect_message(
    get_resources(portfolio,
      resources = c("treecover2000", "lossyear", "greenhouse"),
      vers_lossyear = "GFC-2020-v1.8"
    ),
    "Setting to default value of"
  )

  expect_error(
    get_resources(portfolio,
      resources = c("not_available")
    ),
    "requested resource is not supported"
  )

  portfolio2 <- get_resources(portfolio,
    resources = c("treecover2000", "lossyear", "greenhouse"),
    vers_lossyear = "GFC-2020-v1.8", vers_treecover = "GFC-2020-v1.8"
  )

  expect_message(
    get_resources(portfolio2,
      resources = c("treecover2000", "lossyear", "greenhouse")
    ),
    "The following requested resources are already available: treecover2000, lossyear, greenhouse."
  )
})
