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
  portfolio <- init_portfolio(
    aoi,
    years = 2000:2005,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE
  )

  portfolio <- get_resources(
    portfolio,
    resources = c("gfw_treecover", "gfw_lossyear"),
    vers_treecover = "GFC-2020-v1.8",
    vers_lossyear = "GFC-2020-v1.8"
  )

  expect_message(
    calc_indicators(
      portfolio,
      indicators = "treecover_area",
      min_cover = 10
    ),
    "was not specified. Setting to default value"
  )

  stat <- calc_indicators(
    portfolio,
    indicators = "treecover_area",
    min_size = 5,
    min_cover = 30
  )$treecover_area[[1]]

  expect_equal(
    names(stat),
    c("years", "treecover")
  )
  expect_equal(
    stat$years,
    2000:2005
  )
  expect_equal(
    stat$treecover,
    c(2005.175, 2004.664, 2002.985, 1968.824, 1959.262, 1955.029),
    tolerance = 1e-3
  )

  portfolio <- init_portfolio(
    aoi,
    years = 2000:2005,
    outdir = outdir,
    tmpdir = tmpdir,
    add_resources = TRUE,
    verbose = FALSE
  )

  stat <- calc_indicators(
    portfolio,
    indicators = "treecover_area",
    min_size = 5,
    min_cover = 30
  )$treecover_area[[1]]

  expect_equal(
    names(stat),
    c("years", "treecover")
  )
  expect_equal(
    stat$years,
    2000:2005
  )
  expect_equal(
    stat$treecover,
    c(2005.175, 2004.664, 2002.985, 1968.824, 1959.262, 1955.029),
    tolerance = 1e-3
  )
})

test_that("Parallelization works", {
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
  aoi <- st_as_sf(st_make_grid(aoi, n = 3))

  portfolio <- init_portfolio(
    aoi,
    years = 2000:2005,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE
  )

  portfolio <- get_resources(
    portfolio,
    resources = c("gfw_treecover", "gfw_lossyear"),
    vers_treecover = "GFC-2020-v1.8",
    vers_lossyear = "GFC-2020-v1.8"
  )

  library(future)
  plan(multisession, workers = 2)
  stat <- calc_indicators(
    portfolio,
    indicators = "treecover_area",
    min_size = 5,
    min_cover = 30
  )
  plan(sequential)

  expect_equal(
    names(stat),
    c("assetid", "treecover_area", "x")
  )
  expect_equal(
    nrow(stat),
    9
  )

  stat <- lapply(stat$treecover_area, function(x) x$treecover)
  names(stat) <- 1:length(stat)
  stat <- rowSums(as.data.frame(stat))

  expect_equal(
    stat,
    c(2603.803, 2600.664, 2596.358, 2557.306, 2540.299, 2532.707),
    tolerance = 1e-3
  )
})

test_that(".bind_assets works correctly", {
  # Case 1: All assets return equally shaped tibble
  results <- list(
    tibble::tibble(
      year = c(2010, 2011),
      value = c(0.5, 1.0)
    ),
    tibble::tibble(
      year = c(2010, 2011),
      value = c(0.0, 0.5)
    )
  )

  expected_results <- tibble::tibble(
    .id = c("1", "1", "2", "2"),
    year = c(2010, 2011, 2010, 2011),
    value = c(0.5, 1.0, 0.0, 0.5)
  )

  expect_equal(
    .bind_assets(results),
    expected_results
  )

  # Case 2: All assets return NA
  results <- list(
    NA,
    NA
  )

  expected_results <- tibble::tibble(
    .id = c("1", "2"),
    value = c(NA, NA)
  )

  expect_equal(
    .bind_assets(results),
    expected_results
  )

  # Case 3: Some asset(s) return(s) NA
  results <- list(
    tibble::tibble(
      year = c(2010, 2011),
      value = c(0.5, 1.0)
    ),
    NA
  )

  expected_results <- tibble::tibble(
    .id = c("1", "1", "2"),
    year = c(2010, 2011, NA),
    value = c(0.5, 1.0, NA)
  )

  expect_equal(
    .bind_assets(results),
    expected_results
  )
})


test_that(".prep works correctly", {
  x <- read_sf(
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

  x <- suppressWarnings(st_cast(x, to = "POLYGON"))[1, ]
  x <- st_as_sf(st_make_grid(x, n = 3))

  x <- init_portfolio(
    x,
    years = 2000:2005,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE
  )

  x <- get_resources(
    x,
    resources = c("gfw_treecover", "gfw_lossyear"),
    vers_treecover = "GFC-2020-v1.8",
    vers_lossyear = "GFC-2020-v1.8"
  )

  available_resources <- attr(x, "resources")
  required_resources <- available_indicators("treecover_area")[[1]]$resources
  output <- .prep(x, available_resources, required_resources)

  expect_equal(
    length(output),
    2
  )
  expect_equal(
    names(output),
    c("gfw_treecover", "gfw_lossyear")
  )
  expect_true(
    inherits(output[[1]], "SpatRaster"),
  )


  x2 <- read_sf(list.files(
    system.file("extdata", package = "mapme.biodiversity"),
    pattern = "shell_beach", full.names = TRUE
  ))
  required_resources <- available_indicators("mangroves_area")[[1]]$resources
  output <- .prep(x2, available_resources, required_resources)

  expect_equal(
    length(output),
    1
  )
  expect_equal(
    names(output),
    "gmw"
  )
  output <- output$gmw
  expect_equal(
    length(output),
    2
  )
  expect_equal(
    names(output),
    c("gmw-extent_1996.gpkg", "gmw-extent_2016.gpkg")
  )
  expect_true(
    inherits(output[[1]], "sf")
  )

  expect_error(
    .prep(x, available_resources, list(gmw = "sth")),
    "Resource type 'sth' currently not supported"
  )
})
