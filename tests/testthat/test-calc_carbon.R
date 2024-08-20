test_that("calc_irr_carbon works", {
  x <- read_sf(system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
    package = "mapme.biodiversity"
  ))
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  get_resources(x, get_irr_carbon())
  irr_carbon <- prep_resources(x)[["irr_carbon"]]

  expect_error(calc_irr_carbon(type = "other"))
  cb <- calc_irr_carbon(stats = c("sum", "mean"))

  expect_silent(result <- cb(x, irr_carbon))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 4)
  vars <- c("irr_carbon_total_sum", "irr_carbon_total_mean")
  expect_equal(unique(result$variable), vars)
  vals <- c(406579.29, 1373.57, 407012.885, 1375.04)
  expect_equal(result$value, vals, tolerance = 0.01)

  cb <- calc_irr_carbon(stats = "sum", type = "biomass")
  expect_silent(result <- cb(x, irr_carbon))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, rep("irr_carbon_biomass_sum", 2))
  vals <- c(43832.18, 42947.41)
  expect_equal(result$value, vals, tolerance = 0.01)
})


test_that("calc_vul_carbon works", {
  x <- read_sf(system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
    package = "mapme.biodiversity"
  ))
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  get_resources(x, get_vul_carbon())
  vul_carbon <- prep_resources(x)[["vul_carbon"]]

  expect_error(calc_vul_carbon(type = "other"))
  cb <- calc_vul_carbon(stats = c("sum", "mean"))

  expect_silent(result <- cb(x, vul_carbon))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 4)
  vars <- c("vul_carbon_total_sum", "vul_carbon_total_mean")
  expect_equal(unique(result$variable), vars)
  vals <- c(696439, 2353, 696192, 2352)
  expect_equal(result$value, vals, tolerance = 0.01)

  cb <- calc_vul_carbon(stats = "sum", type = "biomass")
  expect_silent(result <- cb(x, vul_carbon))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, rep("vul_carbon_biomass_sum", 2))
  vals <- c(249356, 248878)
  expect_equal(result$value, vals, tolerance = 0.01)
})

test_that("calc_man_carbon works", {
  x <- read_sf(system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
    package = "mapme.biodiversity"
  ))
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  get_resources(x, get_man_carbon())
  man_carbon <- prep_resources(x)[["man_carbon"]]

  expect_error(calc_man_carbon(type = "other"))
  cb <- calc_man_carbon(stats = c("sum", "mean"))

  expect_silent(result <- cb(x, man_carbon))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 4)
  vars <- c("man_carbon_total_sum", "man_carbon_total_mean")
  expect_equal(unique(result$variable), vars)
  vals <- c(819413, 2768, 819413, 2768)
  expect_equal(result$value, vals, tolerance = 0.01)

  cb <- calc_man_carbon(stats = "sum", type = "biomass")
  expect_silent(result <- cb(x, man_carbon))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, rep("man_carbon_biomass_sum", 2))
  vals <- c(248949, 248949)
  expect_equal(result$value, vals, tolerance = 0.01)
})
