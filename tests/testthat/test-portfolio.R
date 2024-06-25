test_that("portoflio I/O works as expected", {
  mapme_options(verbose = F)
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- suppressWarnings(st_cast(x, to = "POLYGON")[1, ])

  x[["assetid"]] <- 1
  dsn <- tempfile(fileext = ".gpkg")

  expect_error(
    write_portfolio(x, dsn),
    "No calculated indicators have been found"
  )

  indicator <- list(tibble(
    datetime = "2000-01-01",
    variable = "biome",
    unit = "ha",
    value = 1
  ))

  x[["biome"]] <- indicator

  expect_invisible(out <- write_portfolio(x, dsn, quiet = TRUE))
  expect_equal(out, dsn)
  expect_silent(write_portfolio(x, dsn))
  expect_equal(st_layers(dsn)[["name"]], c("metadata", "indicators"))

  meta <- st_read(dsn, layer = "metadata", quiet = TRUE)
  inds <- st_read(dsn, layer = "indicators", quiet = TRUE)

  expect_true(inherits(meta, "sf"))
  expect_true(inherits(inds, "data.frame"))

  vars <- c("assetid", "indicator", "datetime", "variable", "unit", "value")
  expect_true(all(vars %in% names(inds)))

  data <- read_portfolio(dsn, quiet = TRUE)
  vars <- c("WDPAID", "ISO3", "assetid", "biome", "geom")
  expect_true(all(vars %in% names(data)))
  expect_true(inherits(data[["biome"]], "list"))

  file.remove(dsn)

  x[["biome2"]] <- indicator
  write_portfolio(x, dsn, quiet = TRUE)
  data <- read_portfolio(dsn, quiet = TRUE)
  vars <- c("biome", "biome2")
  expect_true(all(vars %in% names(data)))

  file.remove(dsn)

  x2 <- x
  x2[["biome"]] <- list(NULL)
  x2[["biome2"]] <- list(NULL)
  x <- do.call(rbind, list(x, x2))
  x$assetid <- 1:2

  write_portfolio(x, dsn)
  data <- read_portfolio(dsn)
  expect_true(!is.null(data[["biome"]][[1]]))
  expect_true(!is.null(data[["biome2"]][[1]]))
  expect_true(is.null(data[["biome"]][[2]]))
  expect_true(is.null(data[["biome2"]][[2]]))
})

test_that(".check_portfolio works as expected", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  p <- suppressWarnings(st_centroid(x))
  expect_error(.check_portfolio(p), "Only assets of type 'POLYGON' and 'MULTIPOLYGON' are supported.")
  expect_silent(.check_portfolio(x))

  center <- as.numeric(suppressWarnings(st_coordinates(st_centroid(x))))
  srs <- sprintf(
    "+proj=laea +x_0=0 +y_0=0 +lon_0=%s +lat_0=%s",
    center[1], center[2]
  )
  x <- st_transform(x, srs)

  expect_message(
    x <- .check_portfolio(x),
    "CRS of x is not EPSG:4326. Attempting to transform."
  )
  mapme_options(verbose = TRUE)
  expect_message(.check_portfolio(x), "Found a column named 'assetid'.")
})

test_that("portfolio helpers work as expected", {
  mapme_options(verbose = FALSE)
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- suppressWarnings(st_cast(x, to = "POLYGON")[1, ])
  x[["assetid"]] <- 1

  expect_error(.indicators_col(x))

  indicator <- list(tibble(
    datetime = "2000-01-01",
    variable = "biome",
    unit = "ha",
    value = 1
  ))

  x[["biome"]] <- indicator
  x[["biome2"]] <- indicator

  cols <- .indicators_col(x)
  expected <- 5:6
  names(expected) <- c("biome", "biome2")
  expect_equal(cols, expected)

  vars <- c("indicator", "datetime", "variable", "unit", "value")
  out <- portfolio_long(x, names(cols))
  expect_equal(nrow(out), 2)
  expect_true(inherits(out, "sf"))
  expect_true(all(vars %in% names(out)))
  out <- portfolio_long(x, names(cols)[1], drop_geoms = TRUE)
  expect_equal(nrow(out), 1)
  expect_false(inherits(out, "sf"))
  expect_true(all(vars %in% names(out)))

  out <- portfolio_wide(x, names(cols))
  expect_equal(nrow(out), 1)
  vars <- c("assetid", "biome_2000-01-01_biome_ha", "biome2_2000-01-01_biome_ha")
  expect_true(all(vars %in% names(out)))
  expect_true(inherits(out, "sf"))
  out <- portfolio_wide(x, names(cols), drop_geoms = TRUE)
  expect_equal(nrow(out), 1)
  expect_true(all(vars %in% names(out)))
  expect_false(inherits(out, "sf"))
  out <- portfolio_wide(x, names(cols)[1])
  expect_equal(nrow(out), 1)
  expect_true(inherits(out, "sf"))
  expect_true(all(c(vars[1:2], "biome2") %in% names(out)))

  x$biome <- list(NULL)
  expect_silent(portfolio_long(x))
  expect_silent(portfolio_wide(x))
  x$biome2 <- list(NULL)
  expect_warning(portfolio_long(x), "All indicator columns contained 'NULL'.")
  expect_warning(portfolio_wide(x), "All indicator columns contained 'NULL'.")
})
