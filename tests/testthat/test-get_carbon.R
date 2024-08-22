test_that("get_carbon works", {
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)

  mapme_options(outdir = NULL)
  expect_warning(get_irr_carbon())
  expect_warning(get_man_carbon())
  expect_warning(get_vul_carbon())

  mapme_options(outdir = outdir)
  gca <- get_irr_carbon()
  fps <- gca(outdir = file.path(outdir, "irr_carbon"))
  expect_silent(.check_footprints(fps))
  expect_equal(nrow(fps), 6)

  gca <- get_man_carbon()
  fps <- gca(outdir = file.path(outdir, "man_carbon"))
  expect_silent(.check_footprints(fps))
  expect_equal(nrow(fps), 6)

  gca <- get_vul_carbon()
  fps <- gca(outdir = file.path(outdir, "vul_carbon"))
  expect_silent(.check_footprints(fps))
  expect_equal(nrow(fps), 6)
})

test_that(".get_goldstein_url works", {
  skip_on_cran()
  skip_if_not(Sys.getenv("USER") == "darius")
  expect_silent(result <- .get_goldstein_url("Irrecoverable"))
  expect_equal(nrow(result), 2)
  filenames <- c("Irrecoverable_Carbon_2010.zip", "Irrecoverable_Carbon_2018.zip")
  expect_equal(result$filename, filenames)
})
