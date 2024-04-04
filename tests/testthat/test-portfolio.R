test_that("portoflio I/O works as expected", {
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  aoi[["assetid"]] <- 1
  tmpfile <- file.path(tempdir(), "portfolio_out.gpkg")

  expect_error(write_portfolio(aoi, tmpfile, quiet = TRUE), "No calculated indicators have been found")

  aoi[["biome"]] <- list(tibble(A = 1, B = 2))
  expect_invisible(
    write_portfolio(aoi, tmpfile, quiet = TRUE)
  )

  expect_warning(
    write_portfolio(aoi, file.path(tempdir(), "portfolio_out.shp"), overwrite = TRUE, quiet = TRUE),
    "Can only write portfolio as GPKG"
  )

  expect_error(
    write_portfolio(aoi, tmpfile, overwrite = FALSE)
  )

  aoi$assetid <- NULL
  expect_error(
    write_portfolio(aoi, file.path(tempdir(), "portfolio_out2.gpkg"))
  )

  expect_error(read_portfolio(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  ))

  expect_silent(
    portfolio2 <- read_portfolio(tmpfile)
  )
  expect_equal(
    names(portfolio2),
    c("WDPAID", "NAME", "DESIG_ENG", "ISO3", "assetid", "biome", "geom")
  )

  file.remove(tmpfile)
})

test_that(".check_portfolio works as expected", {
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  expect_error(.check_portfolio(aoi), "Some assests are not of type POLYGON.")
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  center <- suppressWarnings(st_coordinates(st_centroid(aoi)))
  srs <- sprintf("+proj=laea +x_0=0 +y_0=0 +lon_0=%s +lat_0=%s", center[, 1], center[, 2])
  aoi2 <- st_transform(aoi, srs)

  expect_message(aoi2 <- .check_portfolio(aoi2), "CRS of x is not EPSG:4326. Attempting to transform.")
  mapme_options(verbose = TRUE)
  expect_message(.check_portfolio(aoi2), "Found a column named 'assetid'.")
})
