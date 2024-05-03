test_that(".get_chirps works", {
  skip_on_cran()
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  outdir <- file.path(tempdir(), "mapme-data")
  .copy_resource_dir(outdir)

  mapme_options(
    outdir = outdir,
    verbose = FALSE,
    testing = TRUE
  )

  gc <- get_chirps(years = 2010)
  urls <- gc(aoi)
  exts <- unique(substr(urls, nchar(urls[1]) - 5, nchar(urls[1])))
  expect_equal(exts, "tif.gz")
  expect_gte(length(urls), 12)
})
