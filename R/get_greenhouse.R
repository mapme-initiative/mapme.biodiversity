#' Downloads Forest greenhouse gas emissions layer from GFW
#'
#' This dataset is described in \url{Harris et al 2021}{https://www.nature.com/articles/s41558-020-00976-6}
#' and includes all relevant ecosystem carbon pools (aboveground biomass,
#' belowground biomass, dead wood, litter, soil) and greenhouse gases (CO2, CH4, N2O).
#' The downloaded layer represents the Mg CO2 equivalent per pixel in order to
#' support emission calculations for custom AOIs. To download the resource users
#' have to set up an API key with \url{Global Forest Watch}{https://globalforestwatch.org/my-gfw/}
#' @param x An sf object returned by init_portfolio
#' @param vers_greenhouse The version of the data set to download, defaults to the latest.
#' @param verbose Logical controlling verbosity.
#' @param rundir A directory where intermediate files are written to.
#' @name Forest_Greenhouse_Gas_Emissions
.get_greenhouse <- function(x,
                            vers_greenhouse = "latest",
                            #api_key_gfw = NA,
                            verbose = TRUE,
                            rundir = tempdir()){

  bbox = st_bbox(x)
  # if(is.na(api_key_gfw)){
  #   stop("For resources 'greenhouse' a valid API key has to be specified.
  #        If you don have registered yet, please visit https://globalforestwatch.org/my-gfw and order an api-key.", call. = FALSE)
  # }

  spatialindex = st_read("https://opendata.arcgis.com/datasets/753016096c1d49f0977e7b62533375ee_0.geojson", quiet = TRUE)
  targets = unlist(st_intersects(st_as_sfc(bbox), spatialindex))
  tileids = spatialindex$tile_id[targets]
  #baseurl = sprintf("https://data-api.globalforestwatch.org/dataset/gfw_forest_carbon_gross_emissions/%s/download/geotiff", vers_greenhouse)
  #urls = sprintf("%s?grid=10/40000&tile_id=%s&pixel_meaning=Mg_CO2e_px&x-api-key=%s", baseurl, tileids, api_key_gfw)
  urls = as.character(spatialindex$Mg_CO2e_px_download[spatialindex$tile_id %in% tileids])
  filenames = file.path(rundir, sprintf("gfw_forest_carbon_gross_emissions_Mg_CO2e_px_%s.tif", tileids))
  if(any(file.exists(filenames))) message("Skipping existing files in output directory.")
  # TODO: Parallel downloads
  .downloadOrSkip(urls, filenames, verbose, check_existence = FALSE)
  # return all paths to the downloaded files
  filenames
}

