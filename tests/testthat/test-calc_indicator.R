test_that("calc_indicator works", {
  .clear_resources()
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )

  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  x <- get_resources(
    x,
    get_gfw_treecover(version = "GFC-2023-v1.11"),
    get_gfw_lossyear(version = "GFC-2023-v1.11")
  )

  stat <- calc_indicators(
    x,
    calc_treecover_area(years = 2000:2005, min_size = 5, min_cover = 30)
  )$treecover_area[[1]]

  expect_equal(names(stat), c("datetime", "variable", "unit", "value"))
  expect_equal(format(stat$datetime, "%Y"), as.character(2000:2005))
  expect_equal(stat$value,
    c(1996.577, 1996.064, 1994.376, 1960.769, 1951.388, 1947.187),
    tolerance = 1e-3
  )
})

test_that("calc_indicator works with MULTIPOLYGON and chunking", {
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- st_cast(x, "MULTIPOLYGON")
  area_ha <- as.numeric(st_area(x)) / 10000 / 2

  expect_true(st_geometry_type(x) == "MULTIPOLYGON")

  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, chunk_size = area_ha, verbose = FALSE)

  x <- get_resources(
    x,
    get_gfw_treecover(version = "GFC-2023-v1.11"),
    get_gfw_lossyear(version = "GFC-2023-v1.11")
  )

  stat <- calc_indicators(
    x,
    calc_treecover_area(years = 2000:2005, min_size = 5, min_cover = 30)
  )

  expect_true(inherits(stat$treecover_area[[1]], "tbl_df"))
})

test_that("Parallelization works", {
  .clear_resources()
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- st_as_sf(st_make_grid(x, n = 3))

  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  # aoi <- get_resources(
  #   aoi,
  #   get_gfw_treecover(version = "GFC-2023-v1.11"),
  #   get_gfw_lossyear(version="GFC-2023-v1.11"))

  library(future)
  plan(multisession, workers = 2)
  stat <- get_resources(
    x,
    get_gfw_treecover(version = "GFC-2023-v1.11"),
    get_gfw_lossyear(version = "GFC-2023-v1.11")
  ) %>%
    calc_indicators(
      calc_treecover_area(years = 2000:2005, min_size = 5, min_cover = 30)
    )
  plan(sequential)

  expect_equal(names(stat)[1:2], c("assetid", "treecover_area"))
  expect_equal(nrow(stat), 9)

  stat <- lapply(stat$treecover_area, function(x) x$value)
  names(stat) <- 1:length(stat)
  stat <- rowSums(as.data.frame(stat))

  expect_equal(
    stat,
    c(2610.735, 2607.605, 2603.340, 2564.097, 2547.076, 2539.395),
    tolerance = 1e-3
  )
})

test_that(".check_indicator_fun works correctly", {
  expect_error(.check_indicator_fun("a"))
  fun <- function(x, name, mode, aggregation) {}
  expect_error(.check_indicator_fun(fun))
  fun <- function(x, name, mode, aggregation, verbose) {}
  expect_silent(.check_indicator_fun(fun))
  fun <- function(x, some_resource, name, mode, aggregation, verbose) {}
  expect_silent(f <- .check_indicator_fun(fun))
  expect_true(inherits(f, "function"))
})

test_that(".get_req_resources works", {
  expect_error(.get_req_resources("a"))
  fun <- function(x, name, mode) {}
  expect_error(.get_req_resources(fun))
  fun <- function(x, some_resource, other_resource, name, mode, verbose) {}
  expect_silent(f <- .get_req_resources(fun))
  expect_equal(f, c("some_resource", "other_resource"))
})

test_that(".check_avail_resources works correctly", {
  expect_silent(.check_avail_resources("some_resource", "some_resource"))
  expect_error(.check_avail_resources("some_resource", c("some_resource", "other_resource")))
  expect_error(.check_avail_resources(list(), "some_resource"))
})

test_that(".add_indicator_column works correctly", {
  .clear_resources()
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )

  indicator <- lapply(1:nrow(x), function(i) tibble(A = 1, b = 2))
  expect_silent(x <- .add_indicator_column(x, indicator, name = "new_indicator"))
  expect_warning(.add_indicator_column(x, indicator, name = "new_indicator"))
  expect_true("new_indicator" %in% names(x))
})

test_that(".check_single_asset works correctly", {
  asset <- read_sf(system.file("extdata", "burundi.gpkg", package = "mapme.biodiversity"))
  expect_warning(out <- .check_single_asset(NULL, asset))
  expect_equal(out, NULL)
  expect_warning(out <- .check_single_asset(try("a" + 1, silent = TRUE), asset))
  expect_equal(out, NULL)
  expect_warning(out <- .check_single_asset(c(1:10), asset))
  expect_equal(out, NULL)
  expect_warning(out <- .check_single_asset(tibble(), asset))
  expect_equal(out, NULL)
  expect_warning(out <- .check_single_asset(tibble(a = 1), asset))
  expect_equal(out, NULL)
  obj <- tibble(datetime = 1, variable = 1, unit = 1, value = 1)
  expect_identical(.check_single_asset(obj, asset), obj)
})
