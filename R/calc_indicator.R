calc_indicator <- function(x, indicator, cores=parallel::detectCores()-1, ...){

  # get arguments from function call and portfolio object
  args = list(...)
  atts = attributes(x)
  outdir = atts$outdir
  tmpdir = atts$tmpdir
  rundir = file.path(tmpdir, indicator)
  cores = atts$cores
  verbose = atts$verbose
  dir.create(rundir, showWarnings = FALSE)
  # retrieve the selected indicator
  selected_indicator = available_indicators(indicator)
  # match function call
  fun = match.fun(selected_indicator[[1]]$name)
  # matching the specified arguments to the required arguments
  params = .check_resource_arguments(selected_indicator, args)
  # append parameters
  params$rundir = rundir
  params$verbose = atts$verbose
  resources = selected_indicator[[1]]$inputs
  x = x[order(st_area(st_convex_hull(x)), decreasing = FALSE), ]

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
        p()
        iddir = file.path(rundir, i)
        dir.create(iddir)
        parameters = params
        parameters$rundir = iddir
        parameters$shp = x[i,]
        for(j in 1:length(resources)){
          new_source =  mapme.biodiversity:::.read_source(parameters$shp, resources[j], iddir, outdir)
          if(!is.null(new_source)){
            parameters = append(parameters, new_source)
            names(parameters)[length(names(parameters))] = names(resources)[j]
          }
        }
        out = do.call(fun, args = parameters)
        out$.id = i
        unlink(iddir, recursive = TRUE, force = TRUE)
        out
      })
    })
    future::plan(future::sequential)

  } else {
    progressr::with_progress({
      p = progressr::progressor(along = 1:nrow(x))
      results = purrr::map(1:nrow(x), function(i){
        p()
        iddir = file.path(rundir, i)
        dir.create(iddir)
        parameters = params
        parameters$rundir = iddir
        parameters$shp = x[i,]
        for(j in 1:length(resources)){
          new_source =  mapme.biodiversity::.read_source(parameters$shp, resources[j], iddir, outdir)
          if(!is.null(new_source)){
            parameters = append(parameters, new_source)
            names(parameters)[length(names(parameters))] = names(resources)[j]
          }
        }
        out = do.call(fun, args = parameters)
        out$.id = i
        unlink(iddir, recursive = TRUE, force = TRUE)
        out
      })
    })
  }

  # cleanup the rundir
  unlink(rundir, recursive = TRUE, force = TRUE)
  # bind results to data.frame
  results = do.call(rbind, results)
  # nest the results
  results %<>%
    nest(!!indicator := !.id)
  # attach results
  x[indicator] = results[indicator]
  # sent sf column to back and return
  x = relocate(x, !!attributes(x)[["sf_column"]], .after = last_col())
  x
}

.read_source <- function(shp, resource, rundir, outdir){

  if(resource == "raster"){
    # create a temporary tile-index
    tindex_file = tempfile(pattern = "tileindex", fileext = ".gpkg", tmpdir = rundir)
    command = sprintf("gdaltindex -t_srs EPSG:4326 %s %s/*.tif", tindex_file, file.path(outdir, names(resource)))
    # print(command)
    system(command, intern = TRUE)

    # retrieve tiles that intersect with the shp extent
    tindex = st_read(tindex_file, quiet = TRUE)
    target_files = tindex$location[unlist(st_intersects(shp, tindex))]
    file.remove(tindex_file)

    if(length(target_files) == 0 ){
      stop("Does not intersect.")
    } else if(length(target_files) == 1){
      out = rast(target_files)
      # out = crop(out, vect(shp))
    } else {
      # create a vrt for multiple targets
      bbox = as.numeric(st_bbox(shp))
      vrt_name = tempfile("vrt", fileext = ".vrt", tempdir = rundir)
      out = vrt(target_files, vrt_name)
      # info = system(sprintf("gdalinfo %s", vrt_name), intern= TRUE)
      # cog_name = tempfile("cog", fileext = ".tif", tmpdir = rundir)
      # # command = sprintf("gdal_translate -of COG -ot %s %s %s", datatype, vrt_name, cog_name)
      # command = sprintf("gdalwarp -of COG -te %s %s %s", paste0(bbox, collapse = " "), vrt_name, cog_name)
      # system(command, intern = TRUE)
      # out = rast(cog_name)
    }

    # crop the source to the extent of the current polygon
    out = tryCatch({
      crop(out, vect(shp), tempdir = rundir)
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

  if(resource == "vector"){
    out = st_read(source, wkt_filter = st_as_text(st_geometry(shp)))
  }
  out
}
