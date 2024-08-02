test_that(".get_gfw_emissions works", {
  skip_on_cran()

  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  ge <- get_gfw_emissions()
  expect_silent(.check_resource_fun(ge))
  expect_silent(fps <- ge(x))
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, "gfw_forest_carbon_gross_emissions_Mg_CO2e_px_20N_080W.tif")
})
