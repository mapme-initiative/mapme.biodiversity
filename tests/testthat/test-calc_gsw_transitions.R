test_that("gsw transitions works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )

  shp <- suppressWarnings(st_cast(shp, to = "POLYGON")[1, ])
  gswt <- calc_gsw_transitions()

  expect_equal(
    gswt(shp, NULL),
    NA
  )

  gsw_transitions <- list.files(system.file("res", "gsw_transitions",
    package = "mapme.biodiversity"
  ), pattern = ".tif$", full.names = TRUE)
  gsw_transitions <- rast(gsw_transitions)
  transitions <- gswt(shp, gsw_transitions)

  transitions_expected <- tibble(
    class = c(
      "Permanent", "New Permanent", "Lost Permanent", "Seasonal",
      "New Seasonal", "Seasonal to Permanent", "Permanent to Seasonal",
      "Ephemeral Permanent", "Ephemeral Seasonal"
    ),
    area = c(
      199.4017921627606142, 388.8460738439933948, 0.3806794781267643,
      1.9034698475368319, 83.9057809752881099, 28.8569105465255689,
      1.0659035851024090, 0.6091238501258194, 21.6236787112284574
    )
  )

  expect_equal(
    transitions$area,
    transitions_expected$area,
    tolerance = 1e-4
  )
})
