test_that("calc_key_biodiversity_areas works", {
  sample_path <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
              package = "mapme.biodiversity")
  x <- read_sf(sample_path)
  x_area <- as.numeric(st_area(x))
  x_area <- x_area / 10000

  .clear_resources()
  outdir <- tempfile()
  dir.create(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)

  get_resources(x, get_key_biodiversity_areas(path = sample_path))
  kbas <- prep_resources(x)[["key_biodiversity_areas"]]

  kb <- calc_key_biodiversity_area()

  expect_null(kb(x, key_biodiversity_areas = NULL))
  expect_null(kb(x, key_biodiversity_areas = head(x, 0)))

  result <- kb(x, kbas)
  expect_silent(.check_single_asset(result))
  expect_equal(result$value, x_area, tolerance = 0.01)

  st_geometry(x) <- st_geometry(x) + 5
  st_crs(x) <- st_crs(4326)
  expect_equal(kb(x, kbas), NULL)

  mapme_options(chunk_size = 10000)
  x <- read_sf(sample_path)
  fname_kba <- system.file("res", "key_biodiversity_areas/kbas.gpkg",
                           package = "mapme.biodiversity")
  res <- get_resources(x, get_key_biodiversity_areas(path = fname_kba))
  kbas <- calc_indicators(res, calc_key_biodiversity_area())
  kbas <- portfolio_long(kbas, drop_geoms = TRUE)

  area_kba <- st_area(read_sf(fname_kba))
  area_kba <- as.numeric(area_kba)
  area_kba <- area_kba / 10000

  expect_equal(
    kbas$value,
    area_kba,
    tolerance = 0.01
  )
})
