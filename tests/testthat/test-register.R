test_that("test register_resource works", {
  name <- "sample"
  type <- "raster"
  source <- "sample_source"
  test_fun <- function() {}
  arguments <- list(
    a = 1,
    b = 2
  )


  expect_error(
    register_resource(),
    "neither name, type, fun or arguments can be NULL"
  )

  expect_error(
    register_resource(
      name = 1, type = type, source = source,
      fun = test_fun, arguments = arguments
    ),
    "name needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, type = "unknown", source = source,
      fun = test_fun, arguments = arguments
    ),
    "type needs to be one of 'vector' or 'raster'"
  )

  expect_error(
    register_resource(
      name = name, type = type, source = 1,
      fun = test_fun, arguments = arguments
    ),
    "source needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, type = type, source = source,
      fun = "test_fun", arguments = arguments
    ),
    "fun needs to be a valid function signature"
  )

  expect_error(
    register_resource(
      name = name, type = type, source = source,
      fun = test_fun, arguments = "arguments"
    ),
    "arguments needs to be a list."
  )
})



test_that("test register_indicator works", {
  name <- "sample"
  resources <- list(gfw_treecover = "raster", gmw = "vector")
  processing_mode <- "asset"
  test_fun <- function() {}
  arguments <- list(
    a = 1,
    b = 2
  )


  expect_error(
    register_indicator(),
    "neither name, resources, fun, arguments, or processing_mode can be NULL"
  )

  expect_error(
    register_indicator(
      name = 1, resources = resources, fun = test_fun,
      processing_mode = processing_mode, arguments = arguments
    ),
    "name needs to be a single charachter string"
  )

  expect_error(
    register_indicator(
      name = name, fun = test_fun, arguments = arguments,
      processing_mode = processing_mode,
      resources = append(resources, list(gfw_lossyear = "unknown"))
    ),
    "the following resources have an unknown type specified"
  )

  expect_error(
    register_indicator(
      name = name, resources = resources, fun = 1,
      processing_mode = processing_mode, arguments = arguments
    ),
    "fun needs to be a valid function signature"
  )

  expect_error(
    register_indicator(
      name = name, resources = resources, fun = test_fun,
      processing_mode = "unknown", arguments = arguments
    ),
    "processing_mode needs to be one of 'asset' or 'portfolio'"
  )

  expect_error(
    register_indicator(
      name = name, resources = resources, fun = test_fun,
      processing_mode = processing_mode, arguments = 1
    ),
    "arguments needs to be a list."
  )
})
