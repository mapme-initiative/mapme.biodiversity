test_that(".get_greenhouse works", {
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  outdir <- file.path(tempdir(), "mapme.biodiversity", "res")
  tmpdir <- tempdir()

  portfolio <- init_portfolio(aoi,
    years = 2000:2020,
    outdir = outdir,
    tmpdir = tmpdir,
    cores = 1,
    add_resources = FALSE,
    verbose = TRUE
  )

  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE

  expect_equal(
    basename(get_resources(portfolio, resources = "greenhouse")),
    "gfw_forest_carbon_gross_emissions_Mg_CO2e_px_20N_080W.tif"
  )
})
