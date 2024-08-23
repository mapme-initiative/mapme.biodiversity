test_that("get_gsw_time_series works", {
  aoi <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
                package = "mapme.biodiversity"
    )
  )

  outdir <- system.file("res", package = "mapme.biodiversity")
  mapme_options(outdir = outdir, verbose = FALSE)
  outdir_gsw <- file.path(outdir, "gsw_time_series")
  years <- 2000:2001

  expect_error(
    get_gsw_time_series(years = years, version = ""),
    "version %in% available_versions is not TRUE"
  )
  expect_error(
    get_gsw_time_series(years = 0),
    "The target years do not intersect with the availability of gsw_time_series."
  )
  expect_warning(
    get_gsw_time_series(years = c(NA, NULL, ".", 2000)),
    "NAs introduced by coercion"
  )
  expect_message(
    get_gsw_time_series(years = c(1900, 2000)),
    "Some target years are not available for gsw_time_series."
  )
  expect_silent(
    get_gsw_time_series(years = c(2000, 2000))
  )

  expect_silent(.check_footprints(get_gsw_time_series(2000:2001)(aoi)))
  expect_silent(.check_footprints(get_gsw_time_series(2000)(aoi)))

  aoi_shift <- aoi
  gsw <- get_gsw_time_series(2000:2001)
  st_geometry(aoi_shift) <- st_geometry(aoi) + c(0, -90)
  st_crs(aoi_shift) <- 4326
  expect_error(
    gsw(aoi_shift),
    "The extent of the portfolio does not intersect with the GSW grid."
  )
})
