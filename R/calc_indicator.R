calc_indicators <- function(x, indicators, ...){
  # check if the requested resource is supported
  required_resources = .check_requested_indicator(indicators)
  # check if any of the requested resources is already locally available
  existing_resources = names(attributes(x)$resources)
  .check_existing_resources(existing_resources, required_resources, needed = TRUE)
  ## TODO: check if we can go parallel here. Problem is when errors occur
  # for one resource and it terminates the complete process. We would have
  # to catch that so other processes can terminate successfully.
  for(indicator in indicators) x = .get_single_indicator(x, indicator, ...)
  x
}


#' Calculation of an indicator
#'
#' This functions let's users calculate on or more biodiversity indicators for a portfolio.
#' @param x A sf object returned by init_portfolio().
#' @param indicator A variable length charcter vector with the indicators to calculate.
#' @param ... Additional arguments required by the requested indicators.
#'
#' @export
#' @importFrom dplyr relocate last_col
#' @importFrom tidyr nest
#' @importFrom future plan multisession sequential
.get_single_indicator <- function(x, indicator, ...){

  # get arguments from function call and portfolio object
  args = list(...)
  atts = attributes(x)
  resource_dir = atts$outdir
  tmpdir = file.path(atts$tmpdir, indicator)
  dir.create(tmpdir, showWarnings = FALSE)
  cores = atts$cores
  verbose = atts$verbose

  # retrieve the selected indicator
  selected_indicator = available_indicators(indicator)
  # match function call
  fun = match.fun(selected_indicator[[1]]$name)
  # matching the specified arguments to the required arguments
  baseparams = .check_resource_arguments(selected_indicator, args)
  # append parameters
  baseparams$verbose = atts$verbose
  resources = selected_indicator[[1]]$inputs

  progressr::handlers(global = TRUE)
  progressr::handlers(
    progressr::handler_progress(
      format = sprintf(" Calculating indicator '%s' [:bar] :percent", indicator),
      clear = FALSE,
      width= 60
    ))

  # apply function with parameters and add hidden id column
  if(cores > 1){
    progressr::with_progress({
      p = progressr::progressor(along = 1:nrow(x))
      future::plan(future::multisession, workers = cores)
      results = furrr::future_map(1:nrow(x), function(i){
        rundir = file.path(tmpdir, i) # create a rundir name
        dir.create(rundir, showWarnings = FALSE) # create the current rundir
        params = baseparams # new parameters object
        params$rundir = rundir # change rundir
        params$shp = x[i,] # enter specific polygon
        # loop to read through the ressource
        #.read_source should return NULL if an error occurs
        for(j in 1:length(resources)){
          new_source = .read_source(params$shp, resources[j], rundir, resource_dir)
          if(!is.null(new_source)){
            params = append(params, new_source)
            names(params)[length(names(params))] = names(resources)[j]
          }
        }
        # call the indicator function with the associated parameters
        out = do.call(fun, args = params)
        p() # progress tick
        out$.id = i # add an id variable
        unlink(rundir, recursive = TRUE, force = TRUE) # delete the current rundir
        out # return
      })
    })
    future::plan(future::sequential)

  } else {
    progressr::with_progress({
      p = progressr::progressor(along = 1:nrow(x))
      results = purrr::map(1:nrow(x), function(i){
        rundir = file.path(tmpdir, i) # create a rundir name
        dir.create(rundir, showWarnings = FALSE) # create the current rundir
        params = baseparams # new parameters object
        params$rundir = rundir # change rundir
        params$shp = x[i,] # enter specific polygon
        # loop to read through the ressource
        #.read_source should return NULL if an error occurs
        for(j in 1:length(resources)){
          new_source = .read_source(params$shp, resources[j], rundir, resource_dir)
          if(!is.null(new_source)){
            params = append(params, new_source)
            names(params)[length(names(params))] = names(resources)[j]
          }
        }
        # call the indicator function with the associated parameters
        out = do.call(fun, args = params)
        p() # progress tick
        out$.id = i # add an id variable
        unlink(rundir, recursive = TRUE, force = TRUE) # delete the current rundir
        out # return
      })
    })
  }

  # cleanup the tmpdir for indicator
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  # bind results to data.frame
  results = do.call(rbind, results)
  # nest the results
  results = nest(results, !!indicator := !.id)
  # attach results
  x[indicator] = results[indicator]
  # sent sf column to back and return
  x = relocate(x, !!attributes(x)[["sf_column"]], .after = last_col())
  x
}

.read_source <- function(shp, resource, rundir, resource_dir){

  resource_type = resource[[1]]
  resource = names(resource)
  if(resource_type == "raster"){
    # create a temporary tile-index
    tindex_file = tempfile(pattern = "tileindex", fileext = ".gpkg", tmpdir = rundir)
    command = sprintf("gdaltindex -t_srs EPSG:4326 %s %s/*.tif", tindex_file, file.path(resource_dir, resource))
    # print(command)
    system(command, intern = TRUE)

    # retrieve tiles that intersect with the shp extent
    tindex = read_sf(tindex_file, quiet = TRUE)
    target_files = tindex$location[unlist(st_intersects(shp, tindex))]
    rm(tindex); gc()
    file.remove(tindex_file)

    if(length(target_files) == 0 ){
      warning("Does not intersect.")
      return(NULL)
    } else if(length(target_files) == 1){
      out = terra::rast(target_files)
      # out = crop(out, vect(shp))
    } else {
      # create a vrt for multiple targets
      bbox = as.numeric(st_bbox(shp))
      vrt_name = tempfile("vrt", fileext = ".vrt", tmpdir = rundir)
      out = terra::vrt(target_files, filename = vrt_name)
    }

    # crop the source to the extent of the current polygon
    out = tryCatch({
      terra::crop(out, terra::vect(shp), tempdir = rundir)
    },
    error = function(cond){
      print(cond)
      warning("Cropping failed.", call. = FALSE)
      return(NULL)
    },
    warning = function(cond){
      print(cond)
      warning("Cropping failed.", call. = FALSE)
      return(NULL)
    })

  }

  if(resource_type == "vector"){
    out = read_sf(source, wkt_filter = st_as_text(st_geometry(shp)))
  }
  out
}
