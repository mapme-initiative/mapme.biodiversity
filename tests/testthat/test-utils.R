test_that(".check_requested_resources works", {
  expect_equal(.check_requested_resources("treecover2000"), NULL)
  expect_error(.check_requested_resources("notavailable"))
  expect_error(.check_requested_resources(c("treecover2000", "notavailable")), "resource is")
  expect_error(.check_requested_resources(c("treecover2000", "notavailable1", "notavailable2")), "resources are")
})


test_that(".check_requested_indicator works", {
  expect_equal(.check_requested_indicator("treecover"), c("treecover2000", "lossyear"))
  expect_equal(.check_requested_indicator(c("treecover", "emissions")), c("treecover2000", "lossyear", "greenhouse"))
  expect_error(.check_requested_indicator("notavailable"))
  expect_error(.check_requested_indicator(c("treecover", "notavailable")), "indicator is")
  expect_error(.check_requested_indicator(c("treecover", "notavailable1", "notavailable2")), "indicators are")
})


test_that(".check_existing_resources works", {
  available_resource <- c("treecover2000", "lossyear")
  expect_message(.check_existing_resources(available_resource, "treecover2000"), "is already available")
  expect_message(.check_existing_resources(available_resource, c("treecover2000", "lossyear")), "are already available")
  expect_equal(.check_existing_resources(available_resource, "treecover2000", needed = T), NULL)
  expect_equal(.check_existing_resources(available_resource, c("treecover2000", "lossyear"), needed = T), NULL)
  expect_equal(.check_existing_resources(available_resource, "srtmelevation"), "srtmelevation")
  expect_error(.check_existing_resources(available_resource, "srtmelevation", needed = TRUE), "required resource is not available")
  expect_equal(.check_existing_resources(available_resource, c("srtmelevation", "accessibility")), c("srtmelevation", "accessibility"))
  expect_error(.check_existing_resources(available_resource, c("srtmelevation", "accessibility"), needed = TRUE), "required resources are not available")
  expect_equal(.check_existing_resources(available_resource, c("treecover2000", "srtmelevation", "accessibility")), c("srtmelevation", "accessibility"))
  expect_message(.check_existing_resources(available_resource, c("treecover2000", "srtmelevation", "accessibility")), "requested resource is already available")
  expect_error(.check_existing_resources(available_resource, c("treecover2000", "srtmelevation", "accessibility"), needed = TRUE), "required resources are not available")
})


test_that(".check_resource_arguments works", {
  resource <- available_resources("treecover2000")
  expect_equal(.check_resource_arguments(resource, args = list()), list(vers_treecover = "GFC-2020-v1.8"))
  expect_message(.check_resource_arguments(resource, args = list()), "Argument 'vers_treecover' for resource 'treecover2000' was not specified")
  expect_equal(.check_resource_arguments(resource, args = list(vers_treecover = "GFC-2020-v1.8")), list(vers_treecover = "GFC-2020-v1.8"))
  expect_equal(.check_resource_arguments(resource, args = list(vers_treecover = "GFC-2020-v1.8", noarg = NA)), list(vers_treecover = "GFC-2020-v1.8"))
})

test_that(".make_global_grid works", {
  standard_grid <- .make_global_grid()
  nrows <- nrow(standard_grid)
  bbox <- st_bbox(standard_grid)
  points <- st_multipoint(matrix(c(-180, -50, -180, 80, 170, 80, 170, -50), ncol = 2, byrow = TRUE))
  bbox_c <- st_bbox(points)
  st_crs(bbox_c) <- st_crs("EPSG:4326")
  expect_equal(nrows, 455)
  expect_equal(bbox, bbox_c)
})


test_that(".get_gfw_tile_id works", {
  gfw_grid <- .make_global_grid()
  expect_equal(.get_gfw_tile_id(gfw_grid[100, ]), "20S_110E")
})

test_that(".check_available_years works", {
  expect_equal(
    .check_available_years(2000:2020, 2000:2020, indicator = "treecover"),
    2000:2020
  )
  expect_equal(
    .check_available_years(2000:2010, 2000:2020, indicator = "treecover"),
    2000:2010
  )
  expect_error(
    .check_available_years(2000:2010, 2011:2020, indicator = "treecover"),
    "The target years do not intersect with the availability of treecover."
  )
  expect_message(
    .check_available_years(2000:2011, 2011:2020, indicator = "treecover"),
    "Some target years are not available for treecover."
  )
  expect_equal(
    .check_available_years(2000:2011, 2011:2020, indicator = "treecover"),
    2011
  )
})

test_that(".check_engine works", {
  expect_error(
    .check_engine(c("zonal", "extract"), c("not_implemented", "not_implemented2")),
    "Please specify only one engine of:"
  )
  expect_error(
    .check_engine(c("zonal", "extract"), "not_implemented"),
    "Engine 'not_implemented' is not an available engine. Please choose one of:"
  )
})


test_that(".check_stats works", {
  expect_error(
    .check_stats(c("mean", "min", "max"), c("mean", "min", "other")),
    "Statistic 'other' is not supported. Please choose one of: mean, min, max"
  )
  expect_error(
    .check_stats(c("mean", "min"), c("mean", "other", "other2")),
    "Statistics 'other', 'other2' are not supported. Please choose one of: mean, min"
  )
})




test_that(".download_or_skip works", {
  skip_on_cran()
  urls <- rep("https://github.com/mapme-initiative/mapme.biodiversity/blob/main/R/utils.R", 3)
  filenames <- sapply(1:3, function(i) tempfile())
  expect_length(
    .download_or_skip(urls, filenames, verbose = TRUE, check_existence = TRUE),
    3
  )
  expect_length(
    .download_or_skip(urls, filenames, verbose = TRUE, check_existence = TRUE),
    3
  )
  file.remove(filenames)
  urls[1] <- paste(urls[1], "nonexisting", sep = "")
  expect_length(
    .download_or_skip(urls, filenames, verbose = TRUE, check_existence = TRUE),
    2
  )
  file.remove(filenames[2:3])
})
