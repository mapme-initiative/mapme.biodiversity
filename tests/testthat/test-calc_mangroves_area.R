test_that("mangrove extent works", {
  shp <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )
  shp <- st_make_valid(shp)
  source <- list.files(system.file("res", "gmw",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  gmw <- lapply(1:length(source), function(j) {
    out <- read_sf(source[[j]])
    out <- st_make_valid(out)
  })
  names(gmw) <- basename(source)
  result <- .calc_mangroves_area(shp, gmw)
  expect_equal(
    names(result),
    c("mangrove_extent", "year")
  )
  expect_equal(
    result$mangrove_extent,
    c(1214.88, 1206.61),
    tolerance = 1e-4
  )
  expect_equal(
    result$year,
    c("1996", "2016")
  )
})
