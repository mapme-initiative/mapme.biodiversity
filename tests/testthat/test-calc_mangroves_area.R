test_that("mangrove extent works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_gmw(years = c(1996, 2016)))
  gmw <- prep_resources(x)[["gmw"]]

  ma <- calc_mangroves_area()
  expect_true(is.null(ma(x, NULL)))
  result <- ma(x, gmw)
  expect_equal(unique(result$variable), "mangroves")
  expect_equal(result$value, c(1214.88, 1206.61), tolerance = 1e-4)
  expect_equal(format(result$datetime, "%Y"), c("1996", "2016"))
})
