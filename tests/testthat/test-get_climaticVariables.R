test_that(".get_climaticVariables works", {
  skip_on_cran()
  aoi <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  aoi <- suppressWarnings(st_cast(aoi, to = "POLYGON")[1, ])

  temp_loc <- file.path(tempdir(), "mapme.biodiversity")
  dir.create(temp_loc, showWarnings = FALSE)
  resource_dir <- system.file("res", package = "mapme.biodiversity")
  file.copy(resource_dir, temp_loc, recursive = TRUE)
  outdir <- file.path(tempdir(), "mapme.biodiversity", "res")
  tmpdir <- tempdir()

  portfolio <- init_portfolio(aoi,
    years = 2000:2018,
    outdir = outdir,
    tmpdir = tmpdir,
    add_resources = FALSE,
    verbose = FALSE
  )
  # Add testing attribute in order to skip downloads
  attributes(portfolio)$testing <- TRUE
  expect_equal(
    .get_climatic_variables(portfolio, "tmin"),
    c("wc2.1_2.5m_tmin_2000-2009.zip", "wc2.1_2.5m_tmin_2010-2018.zip")
  )
  expect_equal(
    .get_climatic_variables(portfolio, "tmax"),
    c("wc2.1_2.5m_tmax_2000-2009.zip", "wc2.1_2.5m_tmax_2010-2018.zip")
  )
  expect_equal(
    .get_climatic_variables(portfolio, "prec"),
    c("wc2.1_2.5m_prec_2000-2009.zip", "wc2.1_2.5m_prec_2010-2018.zip")
  )
})
