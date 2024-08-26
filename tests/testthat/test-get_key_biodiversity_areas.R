test_that("get_key_biodiversity_areas works", {
  sample_path <- system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  aoi <- read_sf(sample_path)
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  expect_error(
    get_resources(aoi, get_key_biodiversity_areas()),
    "Expecting path to point towards an existing file."
  )
  expect_error(
    get_resources(aoi, get_key_biodiversity_areas("")),
    "Expecting path to point towards an existing file."
  )
  expect_error(
    get_resources(aoi, get_key_biodiversity_areas(NULL)),
    "Expecting path to point towards an existing file."
  )
  expect_error(
    get_key_biodiversity_areas(path = "inst/resources/"),
    "Expecting path to point towards an existing file."
  )

  res <- get_resources(aoi, get_key_biodiversity_areas(sample_path))
  expect_equal(
    names(prep_resources(res)),
    "key_biodiversity_areas"
  )
})
