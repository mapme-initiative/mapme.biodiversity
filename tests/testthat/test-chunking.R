test_that(".crosses dateline works", {
  bbox <- c(xmin = -175.0, ymin = -0.1, xmax = 175.0, ymax = 0.1)
  x <- st_as_sf(st_as_sfc(st_bbox(bbox, crs = st_crs("EPSG:4326"))))
  expect_true(.crosses_dateline(x, offset = 10))
  expect_false(.crosses_dateline(x, offset = 9))
})

test_that(".cast_to_polygon works", {
  bbox <- c(xmin = -1.0, ymin = -1., xmax = 1.0, ymax = 1.0)
  x <- st_as_sfc(st_bbox(bbox))
  x <- st_make_grid(x, n = c(2, 2))
  x <- st_as_sf(st_sfc(st_multipolygon(x), crs = st_crs("EPSG:4326")))
  expect_equal(nrow(x), 1)
  expect_true(st_geometry_type(x) == "MULTIPOLYGON")
  x <- .cast_to_polygon(x)
  expect_equal(nrow(x), 4)
  expect_true(unique(st_geometry_type(x)) == "POLYGON")
})

test_that(".calc_bbox_area works", {
  bbox <- c(xmin = 0.0, ymin = 0.0, xmax = 100.0, ymax = 100.0)
  x <- st_as_sf(st_as_sfc(st_bbox(bbox)))
  expect_equal(.calc_bbox_areas(x), 1)
  x <- st_make_grid(x, n = c(2, 2))
  expect_equal(.calc_bbox_areas(x), rep(0.25, 4))
})

test_that(".grid_geom works", {
  bbox <- c(xmin = 0.0, ymin = 0.0, xmax = 100.0, ymax = 100.0)
  x <- st_as_sf(st_as_sfc(st_bbox(bbox)))
  x$assetid <- 1
  y <- .grid_geom(x, chunk_size = 0.1)
  expect_equal(nrow(y), 16)
  y <- .grid_geom(x, chunk_size = 1)
  expect_equal(nrow(y), 1)
})

test_that(".chunk works", {
  x <- st_read("inst/extdata/sierra_de_neiba_478140.gpkg")
  x$assetid <- 1
  area <- .calc_bbox_areas(x)
  x2 <- .chunk(x, chunk_size = area / 5)
  expect_equal(nrow(x2), 4)
})
