test_that("make_global_grid works", {
  standard_grid <- make_global_grid()
  nrows <- nrow(standard_grid)
  bbox <- st_bbox(standard_grid)
  points <- st_multipoint(matrix(c(-180, -50, -180, 80, 170, 80, 170, -50), ncol = 2, byrow = TRUE))
  bbox_c <- st_bbox(points)
  st_crs(bbox_c) <- st_crs("EPSG:4326")
  expect_equal(nrows, 455)
  expect_equal(bbox, bbox_c)
})

test_that(".check_available_years works", {
  expect_equal(
    check_available_years(2000:2020, 2000:2020, indicator = "treecover_area"),
    2000:2020
  )
  expect_equal(
    check_available_years(2000:2010, 2000:2020, indicator = "treecover_area"),
    2000:2010
  )
  expect_error(
    check_available_years(2000:2010, 2011:2020, indicator = "treecover_area"),
    "The target years do not intersect with the availability of treecover."
  )
  expect_message(
    out <- check_available_years(2000:2011, 2011:2020, indicator = "treecover_area"),
    "Some target years are not available for treecover."
  )
  expect_equal(out, 2011)
})

test_that(".download_or_skip works", {
  skip_on_cran()
  urls <- rep("https://github.com/mapme-initiative/mapme.biodiversity/blob/main/R/utils.R", 3)
  filenames <- sapply(1:3, function(i) tempfile())
  expect_length(
    download_or_skip(urls, filenames, verbose = FALSE, check_existence = TRUE),
    3
  )
  expect_length(
    download_or_skip(urls, filenames, verbose = FALSE, check_existence = TRUE),
    3
  )
  file.remove(filenames)
  urls[1] <- paste(urls[1], "nonexisting", sep = "")
  expect_length(
    download_or_skip(urls, filenames, verbose = FALSE, check_existence = TRUE),
    2
  )
  file.remove(filenames[2:3])
})

test_that(".check_namespace works", {
  expect_error(check_namespace("not-a-package"))
  expect_silent(check_namespace("base"))
})


test_that("unzip and remove works", {
  skip_on_cran()

  dir <- tempfile()
  dir.create(dir)
  cnt <- "ABC"
  text <- file.path(dir, "test.txt")
  zip <- file.path(dir, "test.zip")
  gz <- file.path(dir, "test.gz")

  writeLines("ABC", text)
  suppressMessages(utils::zip(zip, text, flags = "-j"))
  suppressMessages(R.utils::gzip(text, gz))

  expect_true(file.exists(zip))
  expect_true(file.exists(gz))
  expect_true(!file.exists(text))

  expect_error(unzip_and_remove(zip = "test.txt"))
  expect_silent(unzip_and_remove(zip, dir, remove = FALSE))
  expect_true(file.exists(text))
  expect_true(file.remove(text))
  expect_silent(unzip_and_remove(gz, dir, remove = FALSE))
  expect_true(file.exists(file.path(dir, "test")))
  expect_silent(unzip_and_remove(zip, dir, remove = TRUE))
  expect_silent(unzip_and_remove(gz, dir, remove = TRUE))
  expect_true(!file.exists(zip))
  expect_true(!file.exists(gz))
})
