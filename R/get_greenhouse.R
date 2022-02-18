.get_greenhouse <- function(x,
                            vers_greenhouse = "v20211022",
                            api_key_gfw = NA,
                            verbose = TRUE,
                            rundir = tempdir()){

  bbox = attributes(x)$bbox
  if(is.na(api_key)){
    stop("For resources 'greenhouse' a valid API key has to be specified.
         If you don have registered yet, please visit https://globalforestwatch.org/my-gfw and order an api-key.", call. = FALSE)
  }

  baseurl = sprintf("https://data-api.globalforestwatch.org/dataset/gfw_forest_carbon_gross_emissions/%s/download/geotiff", vers)
  grid_GFC = .makeGFWGrid()
  tile_ids = st_intersects(st_as_sfc(bbox), grid_GFC)[[1]]
  if(length(tile_ids) == 0) stop("The extent of the portfolio does not intersect with the GFW grid.")
  ids = sapply(tile_ids, function(n) .getGFWTileId(grid_GFC[n,]))

  if(verbose) pb = progress_bar$new(total = length(ids))
  for (id in ids){
    if(verbose) pb$tick(0)
    download_url = sprintf("%s?grid=10/40000&tile_id=%s&pixel_meaning=Mg_CO2e_px&x-api-key=%s", baseurl, id, api_key)
    resp = GET(download_url)
    download.file(download_url, file.path(rundir, sprintf("greenhouse_%s.tif", id)),  quiet = TRUE)
    if(verbose) pb$tick()
  }
  # return all paths to the downloaded files
  list.files(rundir, full.names = T)
}

