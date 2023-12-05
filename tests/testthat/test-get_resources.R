test_that("get_resources works", {
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
    verbose = FALSE
  )

  expect_error(
    get_resources(portfolio,
      resources = c("not_available")
    ),
    "requested resource is not supported"
  )

  portfolio2 <- get_resources(portfolio,
    resources = c("gfw_treecover", "gfw_lossyear", "gfw_emissions"),
    vers_lossyear = "GFC-2020-v1.8", vers_treecover = "GFC-2020-v1.8"
  )

  expect_message(
    get_resources(portfolio2,
      resources = c("gfw_treecover", "gfw_lossyear", "gfw_emissions")
    ),
    "The following requested resources are already available: gfw_treecover, gfw_lossyear, gfw_emissions."
  )
})
