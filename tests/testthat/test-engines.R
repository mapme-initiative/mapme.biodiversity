test_that("check_engine works", {
  expect_error(
    check_engine(c("not_implemented", "not_implemented2")),
    "Please specify only one engine of:"
  )
  expect_error(
    check_engine("not_implemented"),
    "Engine 'not_implemented' is not an available engine. Please choose one of:"
  )
})

test_that("check_stats works", {
  expect_error(
    check_stats(c("mean", "min", "other")),
    "Statistic 'other' is not supported. Please choose one of: "
  )
  expect_error(
    check_stats(c("mean", "other", "other2")),
    "Statistics 'other', 'other2' are not supported. Please choose one of"
  )
})


test_that("select_engine works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  nasa_grace <- list.files(system.file("res", "nasa_grace",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  nasa_grace <- rast(nasa_grace)
  attributes(shp)$years <- 2003:2022

  expect_error(
    select_engine(
      shp,
      nasa_grace,
      stats = c("mean", "sd"),
      engine = "not-implemented",
      name = NULL,
      mode = "asset"
    )
  )

  expect_error(
    select_engine(
      shp,
      nasa_grace,
      stats = "not_implemented",
      engine = "zonal",
      name = NULL,
      mode = "asset"
    )
  )

  result_asset <- select_engine(
    shp, nasa_grace,
    stats = c("mean", "sd"), engine = "zonal",
    name = NULL, mode = "asset"
  )
  result_portfolio <- select_engine(
    shp, nasa_grace,
    stats = c("mean", "sd"), engine = "zonal",
    name = NULL, mode = "portfolio"
  )
  result_name <- select_engine(
    shp, nasa_grace,
    stats = c("mean", "sd"), engine = "zonal",
    name = "drought", mode = "asset"
  )

  expect_true(inherits(result_asset, "tbl_df"))
  expect_true(inherits(result_portfolio, "list"))
  expect_equal(names(result_asset), c("mean", "sd"))
  expect_equal(names(result_name), c("drought_mean", "drought_sd"))
})
