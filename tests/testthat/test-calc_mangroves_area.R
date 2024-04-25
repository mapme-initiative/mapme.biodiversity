test_that("mangrove extent works", {
  x <- read_sf(
    system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
      package = "mapme.biodiversity"
    )
  )
  x <- st_make_valid(x)
  source <- list.files(system.file("res", "gmw",
    package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  gmw <- lapply(1:length(source), function(j) {
    out <- read_sf(source[[j]])
    out <- st_make_valid(out)
  })
  names(gmw) <- basename(source)
  ma <- calc_mangroves_area()
  expect_true(is.null(ma(x, NULL)))
  result <- ma(x, gmw)
  expect_equal(unique(result$variable), "mangroves")
  expect_equal(result$value, c(1214.88, 1206.61), tolerance = 1e-4)
  expect_equal(format(result$datetime, "%Y"), c("1996", "2016"))
})
