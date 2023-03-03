test_that("test parallelization using future", {
  aoi <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
                package = "mapme.biodiversity"
    )
  )
  aoi <- st_make_grid(aoi, n = 2) %>%
    st_as_sf()

  temp_loc <- file.path(tempdir(), "mapme.biodiversity")
  dir.create(temp_loc, showWarnings = FALSE)
  resource_dir <- system.file("res", package = "mapme.biodiversity")
  file.copy(resource_dir, temp_loc, recursive = TRUE)
  outdir <- file.path(tempdir(), "mapme.biodiversity", "res")
  tmpdir <- tempdir()

  portfolio <- init_portfolio(aoi,
                              years = 2000:2005,
                              outdir = outdir,
                              tmpdir = tmpdir,
                              verbose = FALSE
  )

  portfolio <- get_resources(portfolio,
                             resources = c("gfw_treecover", "gfw_lossyear"),
                             vers_treecover = "GFC-2020-v1.8",
                             vers_lossyear = "GFC-2020-v1.8"
  )

  library(future)
  library(progressr)
  if (Sys.info()[["sysname"]] == "Windows"){
    plan(multisession, workers = 2)
  } else {
    plan(multicore, workers = 2)
  }

  expect_silent({
    portfolio <- calc_indicators(portfolio,
                                 indicators = "treecover_area",
                                 min_cover = 10,
                                 min_size = 1
    )
  })
  plan(sequential)
})
