test_that("mapme_options works", {
  opts <- mapme_options()
  names <- c("outdir", "chunk_size", "retries", "verbose", "log_dir")
  expect_equal(names(opts), names)

  expect_error(mapme_options(outdir = 1))
  expect_error(mapme_options(verbose = 1))
  expect_error(mapme_options(chunk_size = "a"))
  expect_error(mapme_options(retries = "a"))
})

test_that("test register_resource works", {
  name <- "sample"
  description <- "simple description"
  type <- "raster"
  source <- "sample_source"
  licence <- "CC-BY"

  expect_error(
    register_resource(),
    "neither name, description, licence, source, nor type can be NULL"
  )

  expect_error(
    register_resource(
      name = 1, description = description, licence = licence, source = source, type = type
    ),
    "name needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, description = 1, licence = licence, source = source, type = type
    ),
    "description needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, description = description, licence = 1, source = source, type = type
    ),
    "licence needs to be a single charachter string"
  )

  expect_error(
    register_resource(
      name = name, description = description, licence = licence, source = 1, type = type
    ),
    "source needs to be a single charachter string"
  )


  expect_error(
    register_resource(
      name = name, description = description, licence = licence, source = source, type = "unknown"
    ),
    "type needs to be one of 'vector' or 'raster'"
  )


  expect_silent(register_resource(name, description, licence, source, type))
  res <- available_resources(name)
  expect_equal(names(res), c("name", "description", "licence", "source", "type"))
  expect_equal(res[["name"]], name)
  expect_equal(res[["description"]], description)
  expect_equal(res[["licence"]], licence)
  expect_equal(res[["source"]], source)
  expect_equal(res[["type"]], type)
})

test_that("test register_indicator works", {
  name <- "sample"
  description <- "sample desc"
  resources <- c("gfw_treecover", "gmw")

  expect_error(
    register_indicator(),
    "neither name, description nor resources can be NULL"
  )

  expect_error(
    register_indicator(
      name = 1, description = description, resources = resources
    ),
    "name needs to be a single charachter string"
  )

  expect_error(
    register_indicator(
      name = name, description = 1, resources = resources
    ),
    "description needs to be a single charachter string"
  )

  expect_error(
    register_indicator(
      name = name,
      description = description,
      resources = 1
    ),
    "resources needs to be a charachter vector"
  )

  expect_silent(register_indicator(name, description, resources))
  ind <- available_indicators(name)
  expect_equal(names(ind), c("name", "description", "resources"))
  res <- ind[["resources"]]
  expect_true(inherits(res, "list"))
  res <- res[[1]]
  expect_true(inherits(res, "tbl_df"))
  expect_equal(nrow(res), 2)
  expect_equal(res[["name"]], resources)
})

test_that("test available_resources works", {
  res <- available_resources()
  expect_true(inherits(res, "tbl_df"))
  expect_true(length(res) > 1)
  expect_error(available_resources("not-available"))
})

test_that("test available_indicators works", {
  ind <- available_indicators()
  expect_true(inherits(ind, "tbl_df"))
  expect_true(nrow(ind) > 1)
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
