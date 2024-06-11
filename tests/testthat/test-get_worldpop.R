test_that(".get_worldpop works", {
  skip_on_cran()
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  expect_error(get_worldpop(1999))
  gwp <- get_worldpop(years = 2010)
  expect_silent(.check_resource_fun(gwp))
  fps <- gwp(outdir = file.path(outdir, "worldpop"))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "ppp_2010_1km_Aggregated.tif")
})
