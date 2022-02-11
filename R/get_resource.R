get_resources <- function(x, resource, ...){

  args = list(...)
  atts = attributes(x)
  # get parallel options and sources
  sources = atts$sources
  if(resource %in% names(sources)){
    stop(sprintf("Resource %s is alread available for the current portfolio at: %s", resource, sources["resource"]))
  }
  # call to get info about all implemented indicator functions
  available_resources = .resources_list()
  # select function information based on names
  if(!resource %in% names(available_resources)){
    stop(sprintf('Resource %s is not a supported resources. Select one of: %s.', resource, paste(names(available_resources), collapse = ", ")))
  }

  selected_resource = available_resources[[resource]]
  # match function
  fun = match.fun(selected_resource$downloader)
  # match arguments, TODO: We need to properly match arguments and catch errors
  # selected_args = args[match.arg(names(selected_resource$arguments), names(args), several.ok = TRUE)]
  selected_args = args[names(selected_resource$arguments)]
  # conduct download function, we can think of an efficient way for parallel downloads
  if(selected_resource$type == "raster"){
    filename = file.path(atts$outdir, paste0(resource, ".tif"))
    if (!file.exists(filename)){
      downloaded_files = fun(st_bbox(x), args, tmpdir = atts$tmpdir)
      .tiffs2COGs(downloaded_files, filename, atts$tmpdir)
    } else {
      message(sprintf("Resource %s exists in output directory. Remove if you wish to re-download", filename))
    }
  } else {
    filename = file.path(atts$outdir, paste0(resource, ".gpkg"))
    if(!file.exists(filename)){
      downloaded_files = fun(st_bbox(x), args, tmpdir = atts$tmpdir)
      .vec2GPKG(downloaded_files, filename, atts$tmpdir)
    } else {
      message(sprintf("Resource %s exists in output directory. Remove if you wish to re-download", filename))
    }
  }

  # add the new resource to the attributes
  if(is.na(atts$sources)){
    atts$sources = filename
    names(atts$sources) = resource
  }
  atts$sources[resource] = filename
  attributes(x) = atts
  x
}


.tiffs2COGs <- function(tifs, filename, tmpdir){

  if(length(tifs)>1){ # we need to create a mosaic first
    command = sprintf("gdal_merge.py -o %s %s", file.path(tmpdir,  "mapme-tmp-mosaic.tif"), paste(tifs, collapse = " "))
    system(command)
    tifs = file.path(tmpdir,  "mapme-tmp-mosaic.tif")
  }

  command = sprintf("gdal_translate %s %s -of COG -co COMPRESS=LZW", tifs, filename)
  system(command)

}

.vec2GPKG <- function(vecs, filename, tmpdir){
  # TODO
  NULL
}
