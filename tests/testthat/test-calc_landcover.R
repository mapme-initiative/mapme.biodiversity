test_that("esa global landcover works", {
  skip_on_cran()
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_esalandcover(years = 2015))
  esalandcover <- prep_resources(x)[["esalandcover"]][[1]]

  cl <- calc_landcover()
  expect_true(is.null(cl(x, NULL)))
  result <- cl(x, esalandcover)
  expect_silent(.check_single_asset(result))
})
