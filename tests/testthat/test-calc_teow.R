test_that("teow computation works", {
  shp <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
                package = "mapme.biodiversity"
    )
  )
  source <- list.files(system.file("res", "ecoregions",
                                   package = "mapme.biodiversity"
  ), pattern = ".gpkg$", full.names = TRUE)
  ecoregions <- lapply(1:length(source), function(j) {
    out <- read_sf(source[[j]])
    out <- st_make_valid(out)
  })
  names(ecoregions) <- basename(source)
  attributes(shp)$cores <- 1
  expect_snapshot(
    suppressWarnings(.calc_teow(shp, ecoregions))
  )
})

