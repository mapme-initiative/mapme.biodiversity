test_that("key biodiversity area works", {
  sample_path <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
              package = "mapme.biodiversity")
  x <- read_sf(sample_path)
  x_area <- st_area(x) |>
    units::set_units("ha") |>
    as.numeric()

  .clear_resources()
  outdir <- tempfile()
  dir.create(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_key_biodiversity_areas(path = sample_path))
  kbas <- prep_resources(x)[["key_biodiversity_areas"]]

  kb <- calc_key_biodiversity_area()
  result <- kb(x, kbas)
  expect_silent(.check_single_asset(result))
  expect_equal(result$value, x_area, tolerance = 0.01)
  # check NULL is returned for 0-length tibbles
  st_geometry(x) <- st_geometry(x) + 5
  st_crs(x) <- st_crs(4326)
  expect_equal(kb(x, kbas), NULL)
})
