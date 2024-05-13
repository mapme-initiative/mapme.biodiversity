test_that("gsw change works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_global_surface_water_change(version = "v1_4_2021"))
  gsw_change <- prep_resources(x)[["global_surface_water_change"]][[1]]

  gswc <- calc_gsw_change()
  expect_true(is.null(gswc(x, NULL)))
  chg <- gswc(x, gsw_change)
  expect_silent(.check_single_asset(chg))
  expect_equal(chg$value, 100, tolerance = 1e-4)
})
