test_that("treecover area and emissions works", {
  x <- read_sf(system.file("extdata", "gfw_sample.gpkg",
    package = "mapme.biodiversity"
  ))

  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(
    x,
    get_gfw_treecover(version = "GFC-2023-v1.11"),
    get_gfw_lossyear(version = "GFC-2023-v1.11"),
    get_gfw_emissions()
  )
  gfw_treecover <- prep_resources(x, resources = "gfw_treecover")[[1]]
  gfw_lossyear <- prep_resources(x, resources = "gfw_lossyear")[[1]]
  gfw_emissions <- prep_resources(x, resources = "gfw_emissions")[[1]]

  years <- 2000:2005
  min_size <- 1
  min_cover <- 10
  tae <- calc_treecover_area_and_emissions(years, min_size, min_cover)
  expect_true(is.null(tae(x, gfw_treecover, gfw_lossyear, NULL)))

  result <- tae(x, gfw_treecover, gfw_lossyear, gfw_emissions)

  expect_equal(unique(result$variable), c("emissions", "treecover"))
  expect_equal(unique(format(result$datetime, "%Y")), as.character(c(2000:2005)))
  expect_snapshot(result$value)

  te <- calc_treecoverloss_emissions(years, min_size, min_cover)
  stats_emissions <- te(x, gfw_treecover, gfw_lossyear, gfw_emissions)
  expect_equal(result$value[result$variable == "emissions"], stats_emissions$value, tolerance = 1e-4)

  # test that emissions and forest loss are returned as 0 if no loss occurs
  gfw_lossyear[gfw_lossyear == 3] <- 1
  stats_treeloss <- tae(x, gfw_treecover, gfw_lossyear, gfw_emissions)
  stats_treeloss <- stats_treeloss[stats_treeloss$variable == "emissions", ]
  expect_equal(stats_treeloss$value[stats_treeloss$datetime == "2003-01-01"], 0)

  years <- 2023:2024
  te <- calc_treecoverloss_emissions(years, min_size, min_cover)
  expect_equal(nrow(te(x, gfw_treecover, gfw_lossyear, gfw_emissions)), 1)

  years <- 2024:2025
  te <- calc_treecoverloss_emissions(years, min_size, min_cover)
  expect_true(is.null(te(x, gfw_treecover, gfw_lossyear, gfw_emissions)))
})
