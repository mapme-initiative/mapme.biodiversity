#' Compute specific indicators
#'
#' With \code{calc_indicators()} specific biodiversity indicators
#' can be calculated. A requirment is that the ressources that
#' are mandatory inputs for the requested indicators are available
#' locally. Multiple indicators and their respective additional
#' arguments can be supplied. You can check available indicators and
#' their requirement via \code{available_indicators()}, but
#' the function will also gracefully inform you about any misspecifications.
#' @param x A biodiversity portfolio object constructed via \code{init_portfolio()}
#' @param indicators A character vector indicating the requested indicators. All
#'   specified indicators must be supported by the package. You can use \code{available_indicators()}
#'   to get more information, e.g. additional required arguments and their default
#'   values, about the supported indicators
#' @param ... Additional arguments required for the requested indicators. Check
#'  \code{available_indicators()} to learn more about the supported indicators and
#'  their arguments.
#' @export
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
#' @keywords internal
#' @importFrom dplyr relocate last_col
#' @importFrom tidyr nest
.get_single_indicator <- function(x, indicator, ...){

  i = NULL
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
  # required resources
  resources = selected_indicator[[1]]$inputs
  # matching the specified arguments to the required arguments
  baseparams = .check_resource_arguments(selected_indicator, args)
  # append parameters
  baseparams$verbose = atts$verbose
  baseparams$tmpdir = tmpdir
  baseparams$resource_dir = resource_dir
  baseparams$fun = fun
  baseparams$resources = resources

  if(verbose){
    progressr::handlers(global = TRUE)
    progressr::handlers(
      progressr::handler_progress(
        format = sprintf(" Calculating indicator '%s' [:bar] :percent", indicator),
        clear = FALSE,
        width= 60
      ))
  }
  # apply function with parameters and add hidden id column
  if(cores > 1){

    if(verbose){
      progressr::with_progress({
        baseparams$p = progressr::progressor(along = 1:nrow(x))
        results = parallel::mclapply(1:nrow(x), function(i){
          .prep_and_compute(x[i,], baseparams, i)
        }, mc.cores = cores)
      })
    } else {

      results = parallel::mclapply(1:nrow(x), function(i){
        .prep_and_compute(x[i,], baseparams, i)
      }, mc.cores = cores)

    }

  } else {

    if(verbose){
      progressr::with_progress({
        baseparams$p = progressr::progressor(along = 1:nrow(x))
        results = lapply(1:nrow(x), function(i){
          .prep_and_compute(x[i,], baseparams, i)
        })
      })
    } else {
      results = lapply(1:nrow(x), function(i){
        .prep_and_compute(x[i,], baseparams, i)
      })
    }
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

#' Internal indicator routine
#'
#' Helper to abstract preparation and computation
#' of indicators per polygon
#' @keywords internal
.prep_and_compute <-  function(shp, params, i){
  rundir = file.path(params$tmpdir, i) # create a rundir name
  dir.create(rundir, showWarnings = FALSE) # create the current rundir
  params$rundir = rundir # change rundir
  params$shp = shp # enter specific polygon
  # loop to read through the ressource
  #.read_source should return NULL if an error occurs
  for(j in 1:length(params$resources)){
    new_source = .read_source(params$shp, params$resources[j], rundir, params$resource_dir)
    if(!is.null(new_source)){
      params = append(params, new_source)
      names(params)[length(names(params))] = names(params$resources)[j]
    }
  }
  # call the indicator function with the associated parameters
  out = do.call(params$fun, args = params)
  if(params$verbose) params$p() # progress tick
  out$.id = i # add an id variable
  unlink(rundir, recursive = TRUE, force = TRUE) # delete the current rundir
  out # return
}
