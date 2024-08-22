test_that("get_ipbes_biomes works", {
  skip_on_cran()
  skip_if_not(Sys.getenv("USER") == "darius")
  gib <- get_ipbes_biomes()
  expect_silent(fps <- gib())
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "IPBES_UoA_biomes_JK.tif")
})

test_that("calc_ipbes_biomes works", {
  x <- read_sf(system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
    package = "mapme.biodiversity"
  ))
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  f <- file.path(outdir, "res", "ipbes_biomes", "IPBES_UoA_biomes_JK.tif")
  ipbes <- rast(f)
  ib <- calc_ipbes_biomes()

  expect_equal(ib(x, NULL), NULL)
  expect_silent(result <- ib(x, ipbes))
  expect_true(inherits(result, "list"))
  result <- result[[1]]
  expect_silent(.check_single_asset(result))
  expect_equal(result$value, c(1184.2016, 1988.4815), tolerance = 0.01)
  vars <- c("tropical_and_subtropical_dry_and_humid_forests", "shelf_ecosystems")
  expect_equal(result$variable, vars)
})
