test_that("biodiversity intactness index works", {
  x <- read_sf(system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
                           package = "mapme.biodiversity"))
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  lbii <- system.file("res", "biodiversity_intactness_index", "lbii.asc",
                      package = "mapme.biodiversity")
  get_resources(x, get_biodiversity_intactness_index(lbii))
  bii <- prep_resources(x)[["biodiversity_intactness_index"]]
  bi <- calc_biodiversity_intactness_index()

  expect_null(
    bi(x, biodiversity_intactness_index = NULL)
  )
  expect_silent(result <- bi(x, bii))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$variable, "biodiversity_intactness_index")
  expect_equal(result$value, 0.91, tolerance = 0.01)
})

