test_that("calc_species_richness works", {
  x <- read_sf(system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
    package = "mapme.biodiversity"
  ))
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  iucn_dir <- system.file("res", "iucn", package = "mapme.biodiversity")
  iucn_files <- list.files(iucn_dir, pattern = "_SR_", full.names = TRUE)

  get_resources(x, get_iucn(iucn_files))
  iucn <- prep_resources(x)[["iucn"]]

  expect_error(calc_species_richness(engine = "other"))
  expect_error(calc_species_richness(stats = "other"))

  csr <- calc_species_richness(stats = "median")
  expect_equal(csr(x, NULL), NULL)
  iucnNA <- iucn
  iucnNA[] <- NA
  expect_equal(csr(x, iucnNA), NULL)

  expect_silent(result <- csr(x, iucn))
  expect_silent(.check_single_asset(result))
  expect_equal(nrow(result), 2)
  vars <- c("amphibians_sr_2023_median", "birds_thr_sr_2023_median")
  expect_equal(unique(result$variable), vars)
  vals <- c(15, 27)
  expect_equal(result$value, vals)
})
