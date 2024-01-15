test_that("gsw transitions works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  portfolio <- init_portfolio(
    shp,
    years = 2024,
    tmpdir =  "~/R/mapme.biodiversity/inst/res/gsw_transitions",
    verbose = TRUE
  )

  gsw_transitions <- list.files(system.file("res", "gsw_transitions",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_transitions <- rast(gsw_transitions)
  transitions <- .calc_gsw_transitions(portfolio, gsw_transitions)

  transitions_expected <- tibble(
    class = c("Permanent", "New Permanent", "Lost Permanent", "Seasonal",
              "New Seasonal", "Seasonal to Permanent", "Permanent to Seasonal",
              "Ephemeral Permanent", "Ephemeral Seasonal"),
    area = c(
      200.0108721014505306, 388.8460738440724072, 0.3806794781267643,
      1.9034698475368321, 83.9057809753045518, 28.8569105465337614,
      1.0659035851027816, 0.6091238501254469, 21.6236787112351578
    )
  )

  expect_equal(
    transitions$area,
    transitions_expected$area,
    tolerance = 1e-4
  )
})
