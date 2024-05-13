test_that("gsw occurrence works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_global_surface_water_occurrence(version = "v1_4_2021"))
  gsw_occurrence <- prep_resources(x)[["global_surface_water_occurrence"]][[1]]

  gswo <- calc_gsw_occurrence(min_occurrence = 10)
  expect_true(is.null(gswo(x, NULL)))
  occ <- gswo(x, gsw_occurrence)
  expect_equal(occ$value, 694.159, tolerance = 1e-4)
})
