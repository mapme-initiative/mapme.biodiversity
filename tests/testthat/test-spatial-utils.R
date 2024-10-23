test_that("prep_resources works correctly", {
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

  x <- get_resources(
    x,
    get_gfw_treecover(version = "GFC-2023-v1.11"),
    get_gfw_lossyear(version = "GFC-2023-v1.11")
  )

  available_resources <- .avail_resources()
  required_resources <- available_indicators("treecover_area")[["resources"]][[1]][["name"]]
  output <- prep_resources(x[1, ], available_resources, required_resources)

  expect_equal(length(output), 2)
  expect_equal(names(output), c("gfw_lossyear", "gfw_treecover"))
  expect_true(inherits(output[[1]], "SpatRaster"))

  x2 <- read_sf(list.files(
    system.file("extdata", package = "mapme.biodiversity"),
    pattern = "shell_beach", full.names = TRUE
  ))

  x2 <- get_resources(x2, get_gmw(years = 2016))

  available_resources <- .avail_resources()
  required_resources <- available_indicators("mangroves_area")[["resources"]][[1]][["name"]]
  output <- prep_resources(x2, available_resources, required_resources)

  expect_equal(length(output), 1)
  expect_equal(names(output), "gmw")
  output <- output$gmw
  expect_equal(length(output), 1)
  expect_equal(names(output), "gmw_v3_2016_vec.gpkg")
  expect_true(inherits(output[[1]], "sf"))
  expect_error(prep_resources(x[1, ], available_resources, "not-available"))
})


test_that("VRT deletion works as expected", {
  .clear_resources()
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
      package = "mapme.biodiversity"
    )
  )

  xb <- st_buffer(x, 10000)
  x2 <- st_as_sf(st_as_sfc(st_bbox(xb)))

  r <- rast(ext(xb), nrows = 100, ncols = 100)
  r[] <- runif(ncell(r))
  f <- tempfile(fileext = ".tif")
  writeRaster(r, f)
  fps <- list(test = make_footprints(f, what = "raster"))

  out <- prep_resources(x, avail_resources = fps, resources = "test", mode = "asset")
  expect_true(inMemory(out$test))
  out <- prep_resources(x2, avail_resources = fps, resources = "test", mode = "portfolio")
  expect_true(file.exists(sources(out$test)))
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
  footprints <- make_footprints(files, what = "raster")
  footprints[["location"]] <- files
  extent1 <- c(xmin = -179.9, xmax = 179.9, ymin = -89.9, ymax = 89.9)
  extent2 <- c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)
  x <- st_as_sfc(st_bbox(extent1, crs = "EPSG:4326"))

  tiled_temporal <- .read_raster(x, footprints)
  expect_equal(names(tiled_temporal), c("2000_tile_1", "2001_tile_1"))
  expect_equal(as.vector(ext(tiled_temporal)), extent2)

  tiled <- .read_raster(x, footprints[grep("2001", footprints$location), ])
  expect_equal(names(tiled), "2001_tile_1")
  expect_equal(as.vector(ext(tiled)), extent2)

  temporal <- .read_raster(x, footprints[grep("tile_12.tif", footprints$location), ])
  extent1[c(1:4)] <- c(90, 180, -45, 0)
  expect_equal(names(temporal), c("2000_tile_12", "2001_tile_12"))
  expect_equal(as.vector(ext(temporal)), extent1)

  single <- .read_raster(x, footprints[grep("2000_tile_10.tif", footprints$location), ])
  extent1[c(1:4)] <- c(-90, 0, -45, 0)
  expect_equal(names(single), "2000_tile_10")
  expect_equal(as.vector(ext(single)), extent1)

  expect_error(.read_raster(x, footprints[1:24, ]))
})

test_that(".read_vector works", {
  if (sf::sf_extSoftVersion()[["GDAL"]] < "3.7.0") skip()

  # copy to directory with w permissions, as ogrinfo otherwise fails on CRAN
  org <- system.file("extdata", "burundi.gpkg", package = "mapme.biodiversity")
  outdir <- tempfile()
  dir.create(outdir)
  v <- file.path(outdir, basename(org))
  file.copy(org, v)

  x <- st_as_sf(st_as_sfc(st_bbox(read_sf(v))))
  fps <- make_footprints(v, what = "vector")
  fps[["location"]] <- v
  fps <- rbind(fps, fps)
  expect_silent(out <- .read_vector(x, fps))
  expect_true(class(out) == "list")
  expect_equal(length(out), 2)
  expect_equal(names(out), rep("burundi.gpkg", 2))
  expect_true(inherits(out[[1]], "sf"))
})

test_that("can get info from zipped vector resource", {
  if (sf::sf_extSoftVersion()[["GDAL"]] < "3.7.0") skip("GDAL < 3.7.0")
  if (packageVersion("sf") == "1.0.18") skip("sf == '1.0.18'")
  f <- system.file("extdata", "gfw_sample.gpkg",
    package = "mapme.biodiversity"
  )
  outdir <- tempfile()
  dir.create(outdir)
  fzip <- tempfile(tmpdir = outdir, fileext = ".zip")
  zip(fzip, f, flags = "-j")
  Sys.chmod(outdir, mode = "555")
  src <- paste0("/vsizip/", fzip, "/", basename(f))
  expect_true(spds_exists(src, what = "vector"))
  expect_length(.vector_info(src, oo = character(0)), 1)
  expect_silent(fps <- make_footprints(src))
  expect_true(inherits(fps, "sf"))
  Sys.chmod(outdir, mode = "777")
  unlink(outdir, recursive = TRUE)
})
