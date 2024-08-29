test_that(".crosses_dateline and .split_dateline works", {
  bbox <- c(xmin = -175.1, ymin = -0.1, xmax = 175.1, ymax = 0.1)
  x <- st_as_sfc(st_bbox(bbox, crs = st_crs("EPSG:4326")))
  x <- st_sf(assetid = 1, geometry = x)
  expect_true(.crosses_dateline(st_geometry(x)[[1]], offset = 10))
  expect_false(.crosses_dateline(st_geometry(x)[[1]], offset = 9))
  expect_silent(x2 <- .split_dateline(x))
  expect_equal(nrow(x2), 2)
})

test_that(".calc_bbox_area works", {
  bbox <- c(xmin = 0.0, ymin = 0.0, xmax = 100.0, ymax = 100.0)
  x <- st_as_sf(st_as_sfc(st_bbox(bbox)))
  expect_equal(.calc_bbox_areas(x), 1)
  x <- st_make_grid(x, n = c(2, 2))
  expect_equal(.calc_bbox_areas(x), rep(0.25, 4))
})

test_that(".try_make_valid works", {
  bbox <- c(xmin = -10.0, ymin = -10.0, xmax = 10.0, ymax = 10.0)
  x <- st_as_sf(st_as_sfc(st_bbox(bbox)), crs = "EPSG:4326")
  grid <- st_sf(geometry = st_make_grid(x, n = 2))

  # invalid geom
  invalid <- st_sf(geometry = st_sfc(st_polygon(x = list(
    matrix(c(unlist(st_geometry(grid)[1]), 0, -10, -10, 0), ncol = 2, byrow = T)
  )), crs = "EPSG:4326"))
  expect_false(st_is_valid(invalid))

  # geom collection
  lst <- st_linestring(matrix(c(-10, -10, 10, 10), ncol = 2, byrow = T))
  col <- st_geometrycollection(x = list(lst, st_geometry(grid)[[2]]))
  col <- st_sf(geometry = st_sfc(col, crs = "EPSG:4326"))
  expect_equal(as.character(st_geometry_type(col)), "GEOMETRYCOLLECTION")
  # try make valid
  expect_silent(x2 <- .try_make_valid(rbind(grid, invalid, col)))
  expect_true(inherits(x2, "sf"))
  expect_equal(nrow(x2), 6)
  expect_equal(as.character(unique(st_geometry_type(x2))), "POLYGON")
})

test_that(".cast_to_polygon works", {
  bbox <- c(xmin = -1.0, ymin = -1., xmax = 1.0, ymax = 1.0)
  x <- st_as_sfc(st_bbox(bbox))
  x <- st_make_grid(x, n = c(2, 2))
  x <- st_sfc(st_multipolygon(x), crs = st_crs("EPSG:4326"))
  x <- st_sf(geometry = x, assetid = 1)
  expect_equal(nrow(x), 1)
  expect_true(st_geometry_type(x) == "MULTIPOLYGON")
  expect_silent(x2 <- .cast_to_polygon(x))
  expect_equal(nrow(x2), 4)
  expect_true(unique(st_geometry_type(x2)) == "POLYGON")
  expect_equal(st_bbox(x), st_bbox(x2))
  expect_equal(st_crs(x), st_crs(x2))
})

test_that(".split_multipolygons works", {
  bbox <- c(xmin = -10.0, ymin = -10.0, xmax = 10.0, ymax = 10.0)
  x <- st_as_sfc(st_bbox(bbox, crs = st_crs("EPSG:4326")))
  x <- st_make_grid(x, n = c(4, 4))
  x <- st_sf(
    assetid = 1, chunked = FALSE,
    geometry = st_sfc(st_multipolygon(x)), crs = "EPSG:4326"
  )
  expect_silent(x2 <- .split_multipolygons(x, chunk_size = .calc_bbox_areas(x) / 16))
  expect_equal(nrow(x2), 16)
  expect_equal(unique(x2$assetid), 1)
  expect_equal(st_bbox(x), st_bbox(x2))
  expect_equal(st_crs(x), st_crs(x2))
})

test_that(".make_grid works", {
  bbox <- c(xmin = -10.0, ymin = -10.0, xmax = 10.0, ymax = 10.0)
  x <- st_sf(st_as_sfc(st_bbox(bbox)),
    crs = st_crs("EPSG:4326"),
    assetid = 1
  )
  x2 <- .make_grid(x, .calc_bbox_areas(x) / 16)
  expect_equal(nrow(x2), 16)
  expect_equal(unique(x2$assetid), 1)
  expect_equal(st_bbox(x), st_bbox(x2))
  expect_equal(st_crs(x), st_crs(x2))
})

test_that(".chunk_geoms works", {
  bbox <- c(xmin = -10.0, ymin = -10.0, xmax = 10.0, ymax = 10.0)
  x <- st_sf(st_as_sfc(st_bbox(bbox)),
    crs = st_crs("EPSG:4326"),
    assetid = 1, chunked = TRUE
  )
  chunk_size <- .calc_bbox_areas(x) / 16
  expect_equal(x, .chunk_geoms(x, chunk_size))
  x$chunked <- FALSE
  expect_silent(x2 <- .chunk_geoms(x, chunk_size))
  expect_equal(nrow(x2), 16)
  expect_equal(st_bbox(x), st_bbox(x2))
  expect_equal(st_crs(x), st_crs(x2))
})

test_that(".finalize_assets works correctly", {
  bbox <- c(xmin = -10.0, ymin = -10.0, xmax = 10.0, ymax = 10.0)
  x <- st_sf(st_as_sfc(st_bbox(bbox)),
    crs = st_crs("EPSG:4326"),
    assetid = 1, var = "variable"
  )
  meta <- st_drop_geometry(x)
  x2 <- x[, "assetid"]
  x2 <- .make_grid(x2, .calc_bbox_areas(x) / 4)
  expect_silent(x3 <- .finalize_assets(x2, meta))
  expect_true(inherits(x3, "sf"))
  expect_equal(x3$assetid, rep(1, 4))
  expect_equal(x3$var, rep("variable", 4))
})

test_that("chunking works correctly", {
  .clear_resources()
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x$assetid <- 1
  x <- .geom_last(x)
  st_geometry(x) <- "geometry"
  x_chunked <- .chunk(x, chunk_size = .calc_bbox_areas(x) / 16)
  expect_equal(st_bbox(x), st_bbox(x_chunked), tolerance = 1e-4)
  expect_equal(st_area(x), sum(st_area(x_chunked)), tolerance = 1e-4)
  expect_equal(nrow(x_chunked), 12)
  expect_equal(x, .chunk(x, chunk_size = .calc_bbox_areas(x) * 10))
  expect_equal(x, .chunk(x, chunk_size = NULL))

  data <- tibble(
    datetime = "2000-01-01",
    variable = "test",
    unit = "ha",
    value = 1
  )

  data <- lapply(1:10, function(i) {
    if (i == 5) {
      return(NULL)
    }
    data
  })

  expect_silent(data2 <- .combine_chunks(data, aggregation = "sum"))
  expect_true(inherits(data2, "tbl_df"))
  expect_equal(nrow(data2), 1)
  expect_equal(data2[["value"]], 9)
  vals <- c()
  for (agg in available_stats) {
    expect_silent(data3 <- .combine_chunks(data, aggregation = agg))
    vals <- c(vals, data3[["value"]])
  }
  expect_equal(vals, c(1, 1, 0, 1, 1, 9, 0))
})
