calc_indicator <- function(x, indicator, cores=parallel::detectCores()-1, ...){

  args = list(...)
  atts = attributes(x)
  # get sources
  sources = atts$resources
  # call to get info about all implemented indicator functions
  implemented_funs = .calc_funs()
  # select function information based on names
  if(!indicator %in% names(implemented_funs)){
    stop(sprintf('"%s" indicator not implemented selected one of %s.', indicator, paste(names(implemented_funs), collapse = ", ")))
  }
  selected_indicator = implemented_funs[[indicator]]
  # match function
  fun = match.fun(selected_indicator$name)
  # get local sources and write to parameters
  sources = sources[names(sources) %in% names(selected_indicator$inputs)]
  args = c(args, sources)
  args$years = atts$years
  # for (input in selected_indicator$inputs) params[[input]] = sources$paths[sources$name == input]

  # apply function with parameters and add hidden id column
  if(FALSE){

    results = mclapply(1:nrow(x), function(i){
      params = args
      params$shp = x[i,]
      for(source in sources){ params[names(source)] = .read_source(source, params$shp) }
      out = do.call(fun, args = params)
      out$.id = i
      out
    }, mc.cores = cores)

  } else {

    results = lapply(1:nrow(x), function(i){
      params = args
      params$shp = x[i,]
      for(j in 1:length(sources)){ params[[names(sources)[j]]] = .read_source(sources[j], params$shp) }
      out = do.call(fun, args = params)
      out$.id = i
      out
    })

  }

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

.read_source <- function(source, shp){
  filext = tools::file_ext(source)
  if(filext == "tif"){
    source = rast(source)
    source = crop(source, vect(shp))
  }
  if(filext == "gpkg"){
    source = st_read(source, wkt_filter = st_as_text(st_geometry(shp)))
  }
  source
}
