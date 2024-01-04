test_that("test register_resource works", {
  name <- "sample"
  type <- "raster"
  source <- "sample_source"
  test_fun <- function() {}


  expect_error(
    register_resource(),
    "neither name, type, nor fun can be NULL"
  )

  expect_error(
    register_resource(
      name = 1, type = type, source = source,
      fun = test_fun
    ),
    "name needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, type = "unknown", source = source,
      fun = test_fun
    ),
    "type needs to be one of 'vector' or 'raster'"
  )

  expect_error(
    register_resource(
      name = name, type = type, source = 1,
      fun = test_fun
    ),
    "source needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, type = type, source = source,
      fun = "test_fun"
    ),
    "fun needs to be a valid function signature"
  )
})



test_that("test register_indicator works", {
  name <- "sample"
  resources <- list(gfw_treecover = "raster", gmw = "vector")
  processing_mode <- "asset"
  test_fun <- function() {}

  expect_error(
    register_indicator(),
    "neither name, resources, fun, nor processing_mode can be NULL"
  )

  expect_error(
    register_indicator(
      name = 1, resources = resources, fun = test_fun,
      processing_mode = processing_mode
    ),
    "name needs to be a single charachter string"
  )

  expect_error(
    register_indicator(
      name = name, fun = test_fun,
      processing_mode = processing_mode,
      resources = append(resources, list(gfw_lossyear = "unknown"))
    ),
    "the following resources have an unknown type specified"
  )

  expect_error(
    register_indicator(
      name = name, resources = resources, fun = 1,
      processing_mode = processing_mode
    ),
    "fun needs to be a valid function signature"
  )

  expect_error(
    register_indicator(
      name = name, resources = resources, fun = test_fun,
      processing_mode = "unknown"
    ),
    "processing_mode needs to be one of 'asset' or 'portfolio'"
  )
})
