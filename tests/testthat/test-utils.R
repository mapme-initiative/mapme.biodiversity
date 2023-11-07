test_that(".check_requested_resources works", {
  expect_equal(.check_requested_resources("gfw_treecover"), NULL)
  expect_error(.check_requested_resources("notavailable"))
  expect_error(.check_requested_resources(c("gfw_treecover", "notavailable")), "resource is")
  expect_error(.check_requested_resources(c("gfw_treecover", "notavailable1", "notavailable2")), "resources are")
})


test_that(".check_requested_indicator works", {
  expect_equal(.check_requested_indicator("treecover_area"), c("gfw_treecover", "gfw_lossyear"))
  expect_equal(.check_requested_indicator(c("treecover_area", "treecoverloss_emissions")), c("gfw_treecover", "gfw_lossyear", "gfw_emissions"))
  expect_error(.check_requested_indicator("notavailable"))
  expect_error(.check_requested_indicator(c("treecover_area", "notavailable")), "indicator is")
  expect_error(.check_requested_indicator(c("treecover_area", "notavailable1", "notavailable2")), "indicators are")
})


test_that(".check_existing_resources works", {
  available_resource <- c("gfw_treecover", "gfw_lossyear")
  expect_message(.check_existing_resources(available_resource, "gfw_treecover"), "is already available")
  expect_message(.check_existing_resources(available_resource, c("gfw_treecover", "gfw_lossyear")), "are already available")
  expect_equal(.check_existing_resources(available_resource, "gfw_treecover", needed = T), NULL)
  expect_equal(.check_existing_resources(available_resource, c("gfw_treecover", "gfw_lossyear"), needed = T), NULL)
  expect_equal(.check_existing_resources(available_resource, "srtmelevation"), "srtmelevation")
  expect_error(.check_existing_resources(available_resource, "srtmelevation", needed = TRUE), "required resource is not available")
  expect_equal(.check_existing_resources(available_resource, c("srtmelevation", "accessibility")), c("srtmelevation", "accessibility"))
  expect_error(.check_existing_resources(available_resource, c("srtmelevation", "accessibility"), needed = TRUE), "required resources are not available")
  expect_message(out <- .check_existing_resources(available_resource, c("gfw_treecover", "srtmelevation", "accessibility")), "requested resource is already available")
  expect_equal(out, c("srtmelevation", "accessibility"))
  expect_error(.check_existing_resources(available_resource, c("gfw_treecover", "srtmelevation", "accessibility"), needed = TRUE), "required resources are not available")
})

test_that(".check_resource_arguments works", {
  resource <- available_resources("gfw_treecover")
  expect_message(out <- .check_resource_arguments(resource, args = list()), "Argument 'vers_treecover' for resource 'gfw_treecover' was not specified")
  expect_equal(out, list(vers_treecover = "GFC-2022-v1.10"))
  expect_equal(.check_resource_arguments(resource, args = list(vers_treecover = "GFC-2021-v1.9")), list(vers_treecover = "GFC-2021-v1.9"))
  expect_equal(.check_resource_arguments(resource, args = list(vers_treecover = "GFC-2021-v1.9", noarg = NA)), list(vers_treecover = "GFC-2021-v1.9"))
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
    .check_available_years(2000:2020, 2000:2020, indicator = "treecover_area"),
    2000:2020
  )
  expect_equal(
    .check_available_years(2000:2010, 2000:2020, indicator = "treecover_area"),
    2000:2010
  )
  expect_error(
    .check_available_years(2000:2010, 2011:2020, indicator = "treecover_area"),
    "The target years do not intersect with the availability of treecover."
  )
  expect_message(
    out <- .check_available_years(2000:2011, 2011:2020, indicator = "treecover_area"),
    "Some target years are not available for treecover."
  )
  expect_equal(out, 2011)
})

test_that(".download_or_skip works", {
  skip_on_cran()
  urls <- rep("https://github.com/mapme-initiative/mapme.biodiversity/blob/main/R/utils.R", 3)
  filenames <- sapply(1:3, function(i) tempfile())
  expect_length(
    .download_or_skip(urls, filenames, verbose = FALSE, check_existence = TRUE),
    3
  )
  expect_length(
    .download_or_skip(urls, filenames, verbose = FALSE, check_existence = TRUE),
    3
  )
  file.remove(filenames)
  urls[1] <- paste(urls[1], "nonexisting", sep = "")
  expect_length(
    .download_or_skip(urls, filenames, verbose = FALSE, check_existence = TRUE),
    2
  )
  file.remove(filenames[2:3])
})
