test_that(".get_gsw works", {
  skip_on_cran()
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  temp_loc <- file.path(tempdir(), "mapme.biodiversity")
  dir.create(temp_loc, showWarnings = FALSE)
  resource_dir <- system.file("res", package = "mapme.biodiversity")
  file.copy(resource_dir, temp_loc, recursive = TRUE)
  outdir <- file.path(tempdir(), "mapme.biodiversity", "res", "gsw_occurrence")
  tmpdir <- tempdir()

  mapme_options(
    outdir = outdir,
    verbose = FALSE,
    testing = TRUE
  )
  expect_error(
    .get_gsw(aoi, statistic = "not-available")
  )

  expect_error(
    .get_gsw(aoi, vers_gsw = "not-available")
  )

  expect_equal(
    basename(.get_gsw(aoi, statistic = "occurrence")),
    "occurrence_80W_20Nv1_4_2021.tif"
  )

  gsw <- get_global_surface_water_change()
  expect_equal(basename(gsw(aoi)), "change_80W_20Nv1_4_2021.tif")

  gsw <- get_global_surface_water_transitions()
  expect_equal(basename(gsw(aoi)), "transitions_80W_20Nv1_4_2021.tif")

  gsw <- get_global_surface_water_seasonality()
  expect_equal(basename(gsw(aoi)), "seasonality_80W_20Nv1_4_2021.tif")

  gsw <- get_global_surface_water_recurrence()
  expect_equal(basename(gsw(aoi)), "recurrence_80W_20Nv1_4_2021.tif")

  gsw <- get_global_surface_water_occurrence()
  expect_equal(basename(gsw(aoi)), "occurrence_80W_20Nv1_4_2021.tif")

  # adds test to check for multiple polygons in the same tile
  splitted_aoi <- st_as_sf(st_make_grid(aoi, n = 2))
  expect_equal(
    basename(.get_gsw(splitted_aoi, statistic = "occurrence")),
    "occurrence_80W_20Nv1_4_2021.tif"
  )
})
