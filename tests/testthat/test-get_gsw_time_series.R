test_that("get_gsw_time_series works", {
  aoi <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
                package = "mapme.biodiversity"
    )
  )

  outdir <- system.file("resources", package = "mapme.indicators")
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
  expect_message(
    get_gsw_time_series(years = 0:2000),
    "Some target years are not available for gsw_time_series."
  )

  gsw <- get_gsw_time_series(2000)
  aoi_shift <- aoi
  st_geometry(aoi_shift) <- st_geometry(aoi) + c(0, -90)
  st_crs(aoi_shift) <- 4326
  expect_error(
    gsw(aoi_shift),
    "The extent of the portfolio does not intersect with the GSW grid."
  )
})
