test_that("biome computation works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- st_make_valid(x)
  source <- list.files(system.file("res", "teow",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  teow <- lapply(1:length(source), function(j) {
    out <- read_sf(source[[j]])
    out <- st_make_valid(out)
  })
  names(teow) <- basename(source)
  cb <- calc_biome()
  result <- cb(x, teow)
  expect_silent(.check_single_asset(result))
  expect_equal(result$value, 18352.24, tolerance = 1e-4)
  # check NULL is returned for 0-length tibbles
  st_geometry(x) <- st_geometry(x) + 5
  st_crs(x) <- st_crs(4326)
  expect_equal(cb(x, teow), NULL)
})
