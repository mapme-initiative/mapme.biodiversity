test_that("biome computation works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  shp <- st_make_valid(shp)
  source <- list.files(system.file("res", "teow",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  teow <- lapply(1:length(source), function(j) {
    out <- read_sf(source[[j]])
    out <- st_make_valid(out)
  })
  names(teow) <- basename(source)
  cb <- calc_biome()
  result <- cb(shp, teow)
  expect_equal(
    names(result),
    c("biomes", "area")
  )
  expect_equal(
    result$biomes,
    "Tropical & Subtropical Coniferous Forests"
  )
  expect_equal(
    result$area,
    18352.24,
    tolerance = 1e-4
  )
  # check NA is returned for 0-length tibbles
  st_geometry(shp) <- st_geometry(shp) + 5
  st_crs(shp) <- st_crs(4326)
  expect_equal(
    cb(shp, teow),
    NA
  )
})
