test_that("deforestation drivers works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  suppressWarnings(get_resources(x, get_fritz_et_al(resolution = 100)))
  drivers <- prep_resources(x)[["fritz_et_al"]]
  x <- st_transform(x, st_crs(drivers))

  cdf <- calc_deforestation_drivers()
  result <- cdf(x, drivers)
  expect_silent(.check_single_asset(result))
  expect_equal(
    unique(result$variable),
    c(
      "commercial_agriculture", "commercial_oil_palm", "managed_forests",
      "mining", "natural_disturbances", "pasture", "roads", "wildfire",
      "other_subsistance_agriculture", "shifting_cultivation"
    )
  )
  expect_equal(sum(result$value), 16809, tolerance = 1e-4)
})
