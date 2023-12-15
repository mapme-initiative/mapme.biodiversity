test_that("init_portfolio works", {
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  temp_loc <- file.path(tempdir(), "mapme.biodiversity")
  dir.create(temp_loc, showWarnings = FALSE)
  resource_dir <- system.file("res", package = "mapme.biodiversity")
  file.copy(resource_dir, temp_loc, recursive = TRUE)
  outdir <- file.path(tempdir(), "mapme.biodiversity", "res")
  tmpdir <- tempdir()

  expect_error(
    init_portfolio(aoi,
      years = 1980:2020,
      outdir = outdir,
      tmpdir = outdir,
      verbose = TRUE
    ),
    "outdir and tmpdir need to point to different directories."
  )

  expect_error(
    init_portfolio(aoi,
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      verbose = TRUE
    ),
    "Some assests are not of type POLYGON."
  )

  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  portfolio <- init_portfolio(aoi,
    years = 1980:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = TRUE
  )

  expect_message(
    init_portfolio(portfolio,
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      verbose = TRUE
    ),
    "'assetid'. Overwritting its values with a unique identifier."
  )

  aoi2 <- st_transform(aoi, st_crs(3857))
  expect_message(
    init_portfolio(aoi2,
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      verbose = TRUE
    ),
    "CRS of x is not EPSG:4326. Attempting to transform."
  )

  expect_error(
    init_portfolio(st_cast(aoi, "MULTILINESTRING"),
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      verbose = TRUE
    ),
    "Some assests are not of type POLYGON."
  )

  expect_error(
    init_portfolio(dplyr::filter(aoi, WDPAID == 1),
      years = 1980:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      verbose = TRUE
    ),
    "x must contain at least one asset."
  )

  expect_warning(
    init_portfolio(aoi,
      years = 2000:2020,
      outdir = outdir,
      tmpdir = tmpdir,
      verbose = TRUE,
      aria_bin = "/no-valid-bin"
    ),
    "Argument 'aria_bin' does not point to a executable aria2 installation."
  )

  portfolio <- init_portfolio(aoi,
    years = 2000:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE,
  ) %>%
    get_resources(c("gfw_treecover", "gfw_lossyear"),
      vers_lossyear = "GFC-2020-v1.8", vers_treecover = "GFC-2020-v1.8"
    )

  tmpfile <- file.path(file.path(tempdir(), "portfolio_out.gpkg"))
  portfolio <- calc_indicators(portfolio, "treecover_area", min_size = 1, min_cover = 30)

  expect_invisible(
    write_portfolio(portfolio, tmpfile, quiet = TRUE)
  )

  expect_silent(
    portfolio2 <- read_portfolio(tmpfile)
  )
  expect_equal(
    names(portfolio2),
    c("WDPAID", "NAME", "DESIG_ENG", "ISO3", "assetid", "treecover_area", "geom")
  )

  file.remove(tmpfile)
})
