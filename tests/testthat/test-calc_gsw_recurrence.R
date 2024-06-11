test_that("gsw recurrence works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_global_surface_water_recurrence(version = "v1_4_2021"))
  gsw_recurrence <- prep_resources(x)[["global_surface_water_recurrence"]][[1]]

  gswr <- calc_gsw_recurrence(min_recurrence = 10)
  expect_true(is.null(gswr(x, NULL)))
  rec <- gswr(x, gsw_recurrence)
  expect_equal(rec$value, 719.590, tolerance = 1e-4)
})
