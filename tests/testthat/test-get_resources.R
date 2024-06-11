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

  mapme_options(
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE,
    testing = FALSE
  )


  expect_error(get_resources(aoi, 12))
  expect_error(get_resources(aoi, mean))
  .clear_resources()
  aoi <- get_resources(
    aoi,
    get_gfw_treecover(version = "GFC-2023-v1.11"),
    get_gfw_lossyear(version = "GFC-2023-v1.11"),
    get_gfw_emissions()
  )

  resources <- .avail_resources()
  expect_equal(names(resources), c("gfw_treecover", "gfw_lossyear", "gfw_emissions"))
  expect_message(
    get_resources(aoi, get_gfw_treecover(version = "GFC-2023-v1.11")),
    "Resource 'gfw_treecover' is already available."
  )
})

test_that("test .check_resource_fun works", {
  expect_error(.check_resource_fun("a"))
  fun <- function(x, name, type, outdir, verbose) {}
  expect_error(.check_resource_fun(fun))
  fun <- function(x, name = "new_res", type, outdir, verbose) {}
  expect_silent(f <- .check_resource_fun(fun))
  expect_true(class(f) == "function")
})

test_that("test .make_path works", {
  outdir <- tempfile()
  dir.create(outdir)
  out <- .make_path(outdir, "test")
  expect_true(file.exists(file.path(outdir, "test")))
  expect_equal(basename(out), "test")
})

test_that("test make_footprints works", {
  temp_loc <- file.path(tempdir(), "mapme.biodiversity")
  dir.create(temp_loc, showWarnings = FALSE)
  resource_dir <- system.file("res", package = "mapme.biodiversity")
  file.copy(resource_dir, temp_loc, recursive = TRUE)
  outdir <- file.path(tempdir(), "mapme.biodiversity", "res")

  tifs <- list.files(file.path(outdir, "worldpop"),
    pattern = ".tif$",
    recursive = TRUE, full.names = TRUE
  )

  fps <- make_footprints(tifs, what = "raster")
  expect_true(inherits(fps, "sf"))
  expect_equal(nrow(fps), length(tifs))
  expect_equal(fps$location, tifs)
})
