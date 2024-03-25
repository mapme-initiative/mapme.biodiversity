test_that("mapme_options works", {
  opts <- mapme_options()
  expect_equal(names(opts), c("outdir", "verbose", "aria_bin", "testing"))

  expect_error(mapme_options(outdir = 1))
  expect_error(mapme_options(verbose = 1))
  expect_error(mapme_options(testing = 1))
})

test_that("test register_resource works", {
  name <- "sample"
  type <- "raster"
  source <- "sample_source"
  licence <- "CC-BY"

  expect_error(
    register_resource(),
    "neither name, type, source or licence can be NULL"
  )

  expect_error(
    register_resource(
      name = 1, type = type, source = source, licence = licence
    ),
    "name needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, type = "unknown", source = source, licence = licence
    ),
    "type needs to be one of 'vector' or 'raster'"
  )

  expect_error(
    register_resource(
      name = name, type = type, source = 1, licence = licence
    ),
    "source needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, type = type, source = source, licence = 1
    ),
    "licence needs to be a single charachter string"
  )

  expect_silent(register_resource(name, type, source, licence))
  res <- available_resources(name)
  expect_equal(names(res), c("name", "type", "source", "licence"))
  expect_equal(res[["type"]], type)
  expect_equal(res[["source"]], source)
  expect_equal(res[["licence"]], licence)
})

test_that("test register_indicator works", {
  name <- "sample"
  resources <- list(gfw_treecover = "raster", gmw = "vector")

  expect_error(
    register_indicator(),
    "neither name nor resources can be NULL"
  )

  expect_error(
    register_indicator(
      name = 1, resources = resources
    ),
    "name needs to be a single charachter string"
  )

  expect_error(
    register_indicator(
      name = name,
      resources = append(resources, list(gfw_lossyear = "unknown"))
    ),
    "the following resources have an unknown type specified"
  )

  expect_silent(register_indicator(name, resources))
  ind <- .pkgenv$indicators[name]
  expect_equal(names(ind), name)
  ind <- ind[[name]]
  expect_equal(names(ind), "resources")
  ind <- ind[["resources"]]
  expect_equal(names(ind), c("gfw_treecover", "gmw"))
  expect_equal(ind[["gfw_treecover"]], "raster")
  expect_equal(ind[["gmw"]], "vector")
})

test_that("test available_resources works", {
  res <- available_resources()
  expect_true(inherits(res, "tbl_df"))
  expect_true(length(res) > 1)
  expect_error(available_resources("not-available"))
})

test_that("test available_indicators works", {
  ind <- available_indicators()
  expect_equal(class(ind), "list")
  expect_true(length(ind) > 1)
  expect_error(available_indicators("not-available"))
})


test_that("add / clear resource works", {
  .clear_resources()
  add <- list(1:10)
  names(add) <- "test_res"
  res <- .avail_resources()
  expect_true(class(res) == "list")
  expect_length(res, 0)
  .add_resource(add)
  res <- .avail_resources()
  expect_true(class(res) == "list")
  expect_length(res, 1)
  expect_equal(names(res), "test_res")
  expect_equal(res[["test_res"]], 1:10)
  .clear_resources()
  res <- .avail_resources()
  expect_length(res, 0)
})
