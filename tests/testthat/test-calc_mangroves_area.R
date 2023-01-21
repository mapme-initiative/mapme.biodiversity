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
  attributes(shp)$cores <- 1
  expect_snapshot(
    .calc_mangroves_area(shp, gmw)
  )
})
