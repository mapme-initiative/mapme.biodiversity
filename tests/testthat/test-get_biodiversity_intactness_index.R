test_that("get_biodiversity_intactness_index works", {
  sample_path <- system.file("extdata",
                             "shell_beach_protected_area_41057_B.gpkg",
                             package = "mapme.biodiversity")

  expect_error(
    get_biodiversity_intactness_index(NULL),
    "Expecting path to point towards an existing '.asc' file."
  )
  expect_error(
    get_biodiversity_intactness_index(),
    "Expecting path to point towards an existing '.asc' file."
  )
  expect_error(
    get_biodiversity_intactness_index(sample_path),
    "Expecting path to point towards an existing '.asc' file."
  )

  x <- read_sf(sample_path)
  fname_bii <- system.file("res", "biodiversity_intactness_index/lbii.asc",
                           package = "mapme.biodiversity")
  bii <- get_resources(x,
    get_biodiversity_intactness_index(fname_bii)
  )
  expect_equal(
    names(prep_resources(bii)),
    "biodiversity_intactness_index"
  )
})
