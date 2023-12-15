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
    c(2656.062, 2652.923, 2648.616, 2609.273, 2592.046, 2584.090),
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
  required_resources <- available_indicators("treecover_area")[[1]][["resources"]]
  output <- .prep_resources(x, available_resources, required_resources)

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
    pattern = "shell_beach", full.names = TRUE )) %>%
    init_portfolio(years = 2016,
                     outdir = outdir,
                     tmpdir = tmpdir,
                     verbose = FALSE) %>%
    get_resources("gmw")
  available_resources <- attr(x2, "resources")
  required_resources <- available_indicators("mangroves_area")[[1]][["resources"]]
  output <- .prep_resources(x2, available_resources, required_resources)

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
    .prep_resources(x, available_resources, list(gmw = "sth")),
    "Resource type 'sth' currently not supported"
  )
})


test_that(".read_raster works correctly", {

  dummy <- terra::rast()
  dummy_splitted <- aggregate(dummy, fact = c(ceiling(nrow(dummy) / 4), ceiling(ncol(dummy) / 4)))
  dummy_splitted[] <- 1:16
  polys <- terra::as.polygons(dummy_splitted) %>% st_as_sf()
  dummies <- lapply(1:nrow(polys), function(i) crop(dummy_splitted, polys[i, ]))
  temp_loc <- tempfile()
  dir.create(temp_loc, showWarnings = FALSE)
  purrr::walk(1:length(dummies), function(i) {
    writeRaster(dummies[[i]], filename = file.path(temp_loc, paste0("2000_tile_", i, ".tif")))
    writeRaster(dummies[[i]], filename = file.path(temp_loc, paste0("2001_tile_", i, ".tif")))
  })

  files <- list.files(temp_loc, full.names = TRUE)
  footprints <- .make_footprints(files)
  x <- st_bbox(dummy) %>% st_as_sfc() %>% st_as_sf()
  extent <- c(-180, 180, -90, 90)
  names(extent) <- c("xmin", "xmax", "ymin", "ymax")

  tiled_temporal <- .read_raster(x, footprints)
  expect_equal(names(tiled_temporal), c("2000_tile_1", "2001_tile_1"))
  expect_equal(as.vector(ext(tiled_temporal)), extent)

  tiled <- .read_raster(x, footprints[grep("2001", footprints$location), ])
  expect_equal(names(tiled), "2001_tile_1")
  expect_equal(as.vector(ext(tiled)), extent)

  temporal <- .read_raster(x, footprints[grep("tile_12.tif", footprints$location), ])
  extent[c(1:4)] <- c(90, 180, -45, 0)
  expect_equal(names(temporal), c("2000_tile_12", "2001_tile_12"))
  expect_equal(as.vector(ext(temporal)), extent)

  single <- .read_raster(x, footprints[grep("2000_tile_10.tif", footprints$location), ])
  extent[c(1:4)] <- c(-90, 0, -45, 0)
  expect_equal(names(single), "2000_tile_10")
  expect_equal(as.vector(ext(single)), extent)

  expect_error(.read_raster(x, footprints[1:24, ]))

})


test_that(".check_single_asset works correctly", {
  expect_warning(out <- .check_single_asset(NA, 1))
  expect_equal(out, NA)
  expect_warning(out <- .check_single_asset(try("a" + 1, silent = TRUE), 1))
  expect_equal(out, NA)
  expect_warning(out <- .check_single_asset(c(1:10), 1))
  expect_equal(out, NA)
  expect_warning(out <- .check_single_asset(tibble(), 1))
  expect_equal(out, NA)
})
