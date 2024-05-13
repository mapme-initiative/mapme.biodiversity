test_that(".get_climaticVariables works", {
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

  gcv <- get_worldclim_min_temperature(years = 2018)
  files <- sprintf("wc2.1_2.5m_tmin_2018-%02d.tif", 1:12)
  expect_equal(gcv(x)[["filename"]], files)

  gcv <- get_worldclim_max_temperature(years = 2018)
  files <- sprintf("wc2.1_2.5m_tmax_2018-%02d.tif", 1:12)
  expect_equal(gcv(x)[["filename"]], files)

  gcv <- get_worldclim_precipitation(years = 2018)
  files <- sprintf("wc2.1_2.5m_prec_2018-%02d.tif", 1:12)
  expect_equal(gcv(x)[["filename"]], files)
})
