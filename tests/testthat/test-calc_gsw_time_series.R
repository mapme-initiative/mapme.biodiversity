test_that("gsw time series works", {
  aoi <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
                package = "mapme.biodiversity"
    )
  )

  outdir <- system.file("resources", package = "mapme.indicators")
  mapme_options(outdir = outdir, verbose = FALSE)
  outdir_gsw <- file.path(outdir, "gsw_time_series")

  gsw_fnames_short <- dir(outdir_gsw, pattern = ".tif$")
  gsw_fnames_long <- sub("v5_", "VER5-0_yearlyClassification", gsw_fnames_short)

  file.copy(
    file.path(outdir_gsw, gsw_fnames_short),
    file.path(outdir_gsw, gsw_fnames_long)
  )

  years <- 2000:2001
  aoi <- aoi %>%
    get_resources(get_gsw_time_series(years))

  res <- aoi %>%
    calc_indicators(
      calc_gsw_time_series(years)
    )
  res <- res$gsw_timeseries [[1]]

  file.remove(
    file.path(outdir_gsw, gsw_fnames_long)
  )

  expect_equal(
    res$datetime,
    rep(as.POSIXct(c("2000-01-01T00:00:00Z", "2001-01-01T00:00:00Z")), 4)
  )

  expect_equal(
    res$variable,
    rep(c("no_observations", "not_water", "seasonal_water", "permanent_water"), each = 2)
  )

  expect_equal(
    res$unit,
    rep("ha", 8)
  )

  expect_equal(
    res$value,
    c(2684.78, 2635.35, 13.99, 0.00, 337.11, 327.85, 129.96, 202.64),
    tolerance = 0.1
  )
})
