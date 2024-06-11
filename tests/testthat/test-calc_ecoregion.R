test_that("ecoregion computation works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_teow())
  teow <- prep_resources(x)[["teow"]]

  ce <- calc_ecoregion()
  result <- ce(x, teow)
  expect_silent(.check_single_asset(result))
  expect_equal(result$value, 18352.24, tolerance = 1e-4)

  # check NA is returned for 0-length tibbles
  st_geometry(x) <- st_geometry(x) + 5
  st_crs(x) <- st_crs(4326)
  expect_equal(ce(x, teow), NULL)
})
