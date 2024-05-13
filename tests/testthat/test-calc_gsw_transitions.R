test_that("gsw transitions works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_global_surface_water_transitions(version = "v1_4_2021"))
  gsw_transitions <- prep_resources(x)[["global_surface_water_transitions"]][[1]]

  gswt <- calc_gsw_transitions()
  expect_true(is.null(gswt(x, NULL)))
  transitions <- gswt(x, gsw_transitions)
  expect_silent(.check_single_asset(transitions))

  transitions_expected <- tibble(
    class = c(
      "Permanent", "New Permanent", "Lost Permanent", "Seasonal",
      "New Seasonal", "Seasonal to Permanent", "Permanent to Seasonal",
      "Ephemeral Permanent", "Ephemeral Seasonal"
    ),
    area = c(
      199.40, 388.84, 0.38,
      1.90, 83.90, 28.859,
      1.06, 0.60, 21.62
    )
  )
  expect_equal(transitions$value, transitions_expected$area, tolerance = 1e-4)
})
