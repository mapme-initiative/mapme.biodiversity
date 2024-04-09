test_that(".get_gfw_emissions works", {
  skip_on_cran()
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  temp_loc <- file.path(tempdir(), "mapme.biodiversity")
  dir.create(temp_loc, showWarnings = FALSE)
  resource_dir <- system.file("res", package = "mapme.biodiversity")
  file.copy(resource_dir, temp_loc, recursive = TRUE)
  outdir <- file.path(tempdir(), "mapme.biodiversity", "res")
  tmpdir <- tempdir()

  mapme_options(
    outdir = outdir,
    tmpdir = tmpdir,
    verbose = FALSE,
    testing = TRUE
  )

  ge <- get_gfw_emissions()
  expect_equal(ge(aoi), "gfw_forest_carbon_gross_emissions_Mg_CO2e_px_20N_080W.tif")
  splitted_aoi <- st_as_sf(st_make_grid(aoi, n = 2))
  expect_equal(
    ge(splitted_aoi),
    "gfw_forest_carbon_gross_emissions_Mg_CO2e_px_20N_080W.tif"
  )
})
