available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")

select_engine <- function(shp, raster, stats, engine){

  if (any(!stats %in% available_stats)){
    stop(paste0("Select only available zonal statistics:\n",
                paste(available_stats, collapse = ", ")))
  }

  extractor <- switch(
    engine,
    "extract" =  .engine_extract,
    "exactextract" = .engine_exact_extract,
    "zonal" = .engine_zonal
  )

  extractor(shp, raster, stats)
}


.engine_zonal <- function(shp, raster, stats){
  results <- purrr::map_dfc(stats, function(stat){
    out <- terra::zonal(
      raster,
      vect(shp),
      fun = stat,
      na.rm = TRUE)
    out <- tibble(unlist(out))
    names(out) <- stat
    out})
  results
}


.engine_extract <- function(shp, raster, stats){
  results <- purrr::map_dfc(stats, function(stat){
    out <- terra::extract(
      raster,
      shp,
      fun = stat,
      na.rm = TRUE,
      ID = FALSE)
    out <- tibble(unlist(out))
    names(out) <- stat
    out})
  results
}


.engine_exact_extract <- function(shp, raster, stats){

  if(!requireNamespace("exactextractr", quietly = TRUE)){
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }

  results <- purrr::map_dfc(stats, function(stat){
    org_stat <- stat
    if (stat %in% c("sd", "var"))
      stat <- ifelse(stat == "sd", "stdev", "variance")

    out <- exactextractr::exact_extract(
      raster,
      shp,
      fun = stat)
    out <- tibble(unlist(out))
    names(out) <- org_stat
    out})
  results
}



