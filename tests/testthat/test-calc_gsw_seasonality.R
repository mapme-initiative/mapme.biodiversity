test_that("gsw seasonality works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_global_surface_water_seasonality(version = "v1_4_2021"))
  gsw_seasonality <- prep_resources(x)[["global_surface_water_seasonality"]][[1]]

  gsws <- calc_gsw_seasonality()
  expect_true(is.null(gsws(x, NULL)))
  season <- gsws(x, gsw_seasonality)
  expect_equal(season$value,
    c(2514.759, 22.461, 18.045, 23.831, 19.872, 2.512, 0.152, 0, 0, 0, 0, 0, 617.104),
    tolerance = 1e-3
  )
})
