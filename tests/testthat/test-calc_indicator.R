test_that("calc_indicator works", {
  aoi <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )

  temp_loc <- file.path(tempdir(), "mapme.biodiversity")
  dir.create(temp_loc, showWarnings = FALSE)
  resource_dir <- system.file("res", package = "mapme.biodiversity")
  file.copy(resource_dir, temp_loc, recursive = TRUE)
  outdir <- file.path(tempdir(), "mapme.biodiversity", "res")
  tmpdir <- tempdir()

  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON"))[1, ]
  portfolio <- init_portfolio(aoi,
    years = 2000:2005,
    cores = 1,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE
  )

  portfolio <- get_resources(portfolio,
    resources = c("treecover2000", "lossyear"),
    vers_treecover = "GFC-2020-v1.8",
    vers_lossyear = "GFC-2020-v1.8"
  )

  expect_message(
    calc_indicators(portfolio,
      indicators = "treecover",
      min_cover = 10
    ),
    "was not specified. Setting to default value"
  )

  stat <- calc_indicators(portfolio,
    indicators = "treecover",
    min_size = 5,
    min_cover = 30
  )$treecover[[1]]

  expect_snapshot(stat)

  cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, 2)

  portfolio <- init_portfolio(aoi,
    years = 2000:2005,
    cores = cores,
    outdir = outdir,
    tmpdir = tmpdir,
    add_resources = TRUE,
    verbose = FALSE
  )

  stat <- calc_indicators(portfolio,
    indicators = "treecover",
    min_size = 5,
    min_cover = 30
  )$treecover[[1]]

  expect_snapshot(stat)
})
