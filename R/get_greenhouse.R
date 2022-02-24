.get_greenhouse <- function(x,
                            vers_greenhouse = "v20211022",
                            api_key_gfw = NA,
                            verbose = TRUE,
                            rundir = tempdir()){

  bbox = st_bbox(x)
  if(is.na(api_key_gfw)){
    stop("For resources 'greenhouse' a valid API key has to be specified.
         If you don have registered yet, please visit https://globalforestwatch.org/my-gfw and order an api-key.", call. = FALSE)
  }

  spatialindex = st_read("https://opendata.arcgis.com/datasets/753016096c1d49f0977e7b62533375ee_0.geojson", quiet = TRUE)
  targets = unlist(st_intersects(st_as_sfc(bbox), spatialindex))
  tileids = spatialindex$tile_id[targets]
  baseurl = sprintf("https://data-api.globalforestwatch.org/dataset/gfw_forest_carbon_gross_emissions/%s/download/geotiff", vers_greenhouse)
  urls = sprintf("%s?grid=10/40000&tile_id=%s&pixel_meaning=Mg_CO2e_px&x-api-key=%s", baseurl, tileids, api_key_gfw)

  # TODO: Parallel downloads
  if(verbose) pb = progress_bar$new(total = length(tileids))
  for (i in 1:length(urls)){
    if(verbose) pb$tick(0)
    download.file(urls[i], file.path(rundir, sprintf("gfw_forest_carbon_gross_emissions_Mg_CO2e_px_%s.tif", tileids[i])),  quiet = TRUE)
    if(verbose) pb$tick()
  }
  # return all paths to the downloaded files
  list.files(rundir, full.names = T)
}

