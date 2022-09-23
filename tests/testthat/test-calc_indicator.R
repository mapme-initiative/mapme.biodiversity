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
    resources = c("gfw_treecover", "gfw_lossyear"),
    vers_treecover = "GFC-2020-v1.8",
    vers_lossyear = "GFC-2020-v1.8"
  )

  expect_message(
    calc_indicators(portfolio,
      indicators = "treecover_area",
      min_cover = 10
    ),
    "was not specified. Setting to default value"
  )

  stat <- calc_indicators(portfolio,
    indicators = "treecover_area",
    min_size = 5,
    min_cover = 30
  )$treecover_area[[1]]

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
    indicators = "treecover_area",
    min_size = 5,
    min_cover = 30
  )$treecover_area[[1]]

  expect_snapshot(stat)

  expect_warning(
    calc_indicators(portfolio, "treecover")
  )
})

test_that("calc_indicator handles non-intersecting regions correctly", {
  coords <- data.frame (
    lon = c(-43.59019, -44.42497),
    lat = c(-22.75776, -25.50908)
  )

  pts <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
  x <- st_buffer(pts, c(1000, 10000))
  res <- x %>% init_portfolio(2000:2021,
    cores = 1,
    verbose = FALSE) %>%
  get_resources(
    resources = c("teow")
  ) %>% calc_indicators("ecoregion")

  expect_lt(res$ecoregion[[1]]$area * 1e4 - as.numeric(st_area(x)[1]), 1e-3)
  expect_equal(res$ecoregion[[2]]$area, NA_real_)
})
