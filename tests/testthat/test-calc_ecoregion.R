test_that("ecoregion computation works", {
  skip_on_os("mac")
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
  attributes(shp)$cores <- 1
  expect_snapshot(
    .calc_ecoregion(shp, teow)
  )
})

