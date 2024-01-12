.vector_footprint <- function(src, opts = NULL) {

  info <- sf::gdal_utils("ogrinfo", src, options = c("-json", "-so", "-ro", "-nomd", "-nocount", opts), quiet = TRUE)
  if (length(info) == 0) return(NULL)
  info <- jsonlite::parse_json(info)
  layers <- info[["layers"]]

  layers_bbox <- purrr::map_dfr(layers, function(layer){
    crs <- layer[["geometryFields"]][[1]][["coordinateSystem"]][["wkt"]] %>% st_crs()
    bbox <- layer[["geometryFields"]][[1]][["extent"]] %>% as.numeric()
    class(bbox) <- "bbox"

    bbox <- st_as_sf(st_as_sfc(bbox))
    st_crs(bbox) <- crs
    bbox
  })

  crs <- st_crs(layers_bbox)
  bbox <- st_union(layers_bbox) %>%
    st_as_sf() %>%
    dplyr::rename(geometry = "x") %>%
    dplyr::mutate(source = src)

  st_crs(bbox) <- crs
  bbox[ , "source"]
}

.raster_footprint <- function(src, opts = NULL){

  info <- sf::gdal_utils("gdalinfo", src, options = c("-json", "-norat", "-noct", "-nomd", opts), quiet = TRUE)
  info <- jsonlite::parse_json(info)
  crs <- info[["coordinateSystem"]][["wkt"]] %>% st_crs()


  bbox <- info[["wgs84Extent"]] %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>%
    read_sf() %>%
    dplyr::mutate(source = src)
  st_crs(bbox) <- st_crs(4326)
  bbox <- st_transform(bbox, crs)

  if(st_is_empty(bbox)) {
    coords <- info$cornerCoordinates
    bbox <- st_bbox(c(
      xmin = coords$lowerLeft[[1]],
      xmax = coords$upperRight[[1]],
      ymin=coords$lowerLeft[[2]],
      ymax=coords$upperLeft[[2]]), crs = crs) %>%
      st_as_sfc() %>%
      st_as_sf() %>%
      dplyr::mutate(source = src)
  }
  bbox[ , "source"]
}


.set_precision <- function(data, precision = 1e5){
  crs <- st_crs(data)
  geoms <- st_geometry(data) %>%
    st_sfc(precision = precision) %>%
    st_as_binary() %>%
    st_as_sfc()
  st_geometry(data) <- geoms
  st_geometry(data) <- "geometry"
  st_crs(data) <- crs
  data
}


.get_spds <- function(
    src, dest, opts = NULL, what = c("vector", "raster"),
    gdal_config_global = get_mapme_gdal_config(),
    gdal_config_resource = list()) {

  what <- match.arg(what)
  withr::with_envvar(gdal_config_global, code = if(spds_exists(dest)) return(TRUE))

  util <- switch(what, vector = "vectortranslate", raster = "translate")

  if (length(gdal_config_resource) == 0) { # no resource options
    withr::with_envvar(gdal_config_global, code = {
      if(spds_exists(dest)) return(TRUE)
      try(sf::gdal_utils(
        util = util,
        source = src,
        destination = dest,
        options = opts))
    })
  } else { # roundtrip to tempfile if there are resource options
    tmp <- tempfile(fileext = paste0(".", tools::file_ext(src)))
    withr::with_envvar(gdal_config_resource, code = {
      try(sf::gdal_utils(
        util = util,
        source = src,
        destination = tmp,
        opts = opts))
    })
    withr::with_envvar(gdal_config_global, code = {
      try(sf::gdal_utils(
        util = util,
        source = tmp,
        destination = dest))
    })
  }
  withr::with_envvar(gdal_config_global, code = return(spds_exists(dest)))
}

.prep_resources <- function(x, avail_resources, req_resources) {
  if (any(!names(req_resources) %in% names(avail_resources))) {
    stop("Some required resources are not available.")
  }
  purrr::imap(req_resources, function(resource_type, resource_name) {
    reader <- switch(
      resource_type,
      raster = .read_raster,
      vector = .read_vector,
      stop(sprintf("Resource type '%s' currently not supported", resource_type)))
    gdal_config <- attributes(avail_resources[[resource_name]])[["gdal_config"]]
    withr::with_envvar(gdal_config, reader(x, avail_resources[[resource_name]]))
  })
}


.get_intersection <- function(x, tindex){

  org <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(org)))

  suppressMessages(targets <- st_intersects(x, tindex, sparse = FALSE))
  tindex[which(colSums(targets) > 0), ]

}

.read_vector <- function(x, tindex) {

  matches <- .get_intersection(x, tindex)

  if (nrow(matches) == 0) {
    warning("No intersection with asset.")
    return(NULL)
  }

  vectors <- purrr::map(matches[["location"]], function(loc) {
    tmp <- try(read_sf(loc, wkt_filter = st_as_text(st_as_sfc(st_bbox(x)))), silent = TRUE)
    if (inherits(tmp, "try-error")) { warning(tmp); return(NULL) }
    if (nrow(tmp) == 0) return(NULL)
    st_make_valid(tmp)
  })

  is_null <- unlist(lapply(vectors, is.null))
  vectors <- vectors[!is_null]
  names(vectors) <- basename(matches[["location"]])[!is_null]
  vectors
}


.read_raster <- function(x, tindex) {

  if (st_crs(x) != st_crs(tindex)) {
    x <- st_transform(x, st_crs(tindex))
  }

  matches <- .get_intersection(x, tindex)

  if (nrow(matches) == 0) {
    warning("No intersection with asset.")
    return(NULL)
  }

  geoms <- matches[["geometry"]]
  unique_geoms <- unique(geoms)
  grouped_geoms <- match(geoms, unique_geoms)
  names(grouped_geoms) <- matches[["location"]]
  grouped_geoms <- sort(grouped_geoms)

  n_tiles <- length(unique(grouped_geoms))
  n_timesteps <- unique(table(grouped_geoms))

  if (length(n_timesteps) > 1) {
    stop("Did not find equal number of tiles per timestep.")
  }

  rasters <- lapply(1:n_timesteps, function(i){
    index <- rep(FALSE, n_timesteps)
    index[i] <- TRUE
    filenames <- names(grouped_geoms[index])
    layer_name <- tools::file_path_sans_ext(basename(filenames[1]))
    vrt_name <- tempfile(fileext = ".vrt")
    tmp <- terra::vrt(filenames, filename = vrt_name, set_names = TRUE)
    tmp
  })

  rasters <- do.call(c, rasters)

  # crop the source to the extent of the current polygon
  cropped <- try(terra::crop(rasters, terra::vect(x), snap = "out"))

  if (inherits(cropped, "try-error")) {
    warning(as.character(cropped))
    return(NULL)
  }

  cropped
}


#' Check if a spatial data sets exists
#'
#' This function uses a file path readable by GDAL to check if it can query
#' it for information. Note, this should also work for remote files, e.g.
#' in an S3 bucket. You can use this function in your custom resource
#' function to query if a file is already present at the destination.
#' Note, that performance will be dependent on your connection to the
#' server. It can also be used for files on the local file system.
#'
#' @param path A length 1 character vector with a GDAL readable file path.
#' @param what A character vector indicating if the resource is a vector or raster file.
#'
#' @return A logical, TRUE if the file exists, FALSE if it does not.
#' @export
#'
#' @examples
#'
#' #' # a vector resource
#' vec <- system.file("shape/nc.shp", package="sf")
#' spds_exists(vec, what = "vector")
#'
#' # a raster resource
#' ras <- system.file("ex/elev.tif", package="terra")
#' spds_exists(ras, what = "raster")
#'
#' # a non existing file
#' spds_exists("not-here.gpkg", what = "vector")

spds_exists <- function(
    path,
    what = c("vector", "raster")){
  what <- match.arg(what)

  info <- switch(
    what,
    vector = sf::gdal_utils("ogrinfo", path,
                            options = c("-json", "-ro", "-so", "-nomd",
                                        "-nocount", "-noextent", "-nogeomtype"),
                            quiet = TRUE),
    raster = sf::gdal_utils("gdalinfo", path,
                            options = c("-json", "-nomd", "-norat", "-noct"),
                            quiet = TRUE))

  if (length(info) == 0) return(FALSE) else return(TRUE)
}


#' Create footprints for vector or raster data sets
#'
#' With this function you can create footprints for vector or raster datasets.
#' Specify a character vector of GDAL readable sources of either vector
#' or raster type. Internally, GDAL will be used to create an sf object
#' with a single column indicating the source and the geometry indicating
#' the bounding box of the respective source.
#' Note, the performance for remote sources is dependent on your connection to
#' the server. If you have other means to create footprints in your resource
#' function (e.g. by using the output `{rstac::items_bbox()}`) you should prefer
#' those means over this function for remote files.
#'
#' @param srcs A character vector with GDAL readable paths to either vector
#'  or raster sources, then internal footprint functions are called, or an
#'  sf object which will be appended for filenames and potential options.
#' @param filenames A charcter vector indicating the filenames of the source
#'   data sets if they were written to a destionation. Defaults to `basename(srcs)`
#'   in case of character type or `basename(srcs[["source"]])` in case of
#'   an sf object.
#' @param what A character vector indicating if the files are vector or raster
#'   files.
#' @param opts Either a list or a character vector with opening options (-oo)
#'   of the respective GDAL driver. A list must have equal length of the
#'   input sources, a vector will be recycled.
#'
#' @return An sf object with a the files sources and the geometry indicating
#'   their spatial footprint.
#' @export
#'
#' @examples
#'
#' # a vector resource
#' vec <- system.file("shape/nc.shp", package="sf")
#' make_footprints(vec, what = "vector")
#'
#' # a raster resource
#' ras <- system.file("ex/elev.tif", package="terra")
#' make_footprints(ras, what = "raster")
make_footprints <- function(
    srcs,
    filenames = if(inherits(srcs, "sf")) basename(srcs[["source"]]) else basename(srcs),
    what = c("vector", "raster"),
    opts = NULL) {


  stopifnot(is.null(opts) || (inherits(opts, "list") | inherits(opts, "character")))
  stopifnot(inherits(srcs, "sf") | inherits(srcs, "character"))
  stopifnot(inherits(filenames,  "character") | is.null(filenames))

  n <- ifelse(inherits(srcs, "sf"), nrow(srcs), length(srcs))
  if(length(filenames) != n) stop("filenames required to be of equal length of sources.")

  if(inherits(opts, "list") && length(opts) != n) {
    stop("Opening option list required to be equal length of sources.")
  }

  if(inherits(opts, "character")) {
    opts <- lapply(seq_len(n), function(i) opts)
  }

  if(is.null(opts)){
    opts <- rep(list(opts), n )
  }

  if(inherits(srcs, "character")) {
    what <- match.arg(what)
    srcs <- switch(
      what,
      vector = purrr::map2(srcs, opts,   function(src, opt) .vector_footprint(src, opt)),
      raster = purrr::map2(srcs, opts, function(src, opt) .raster_footprint(src, opt)),
      stop("Can make footprints for vector and raster data only.")
    )
    srcs <- purrr::list_rbind(srcs)
  }

  srcs <- st_as_sf(tibble::as_tibble(srcs))
  srcs[["filename"]] <- filenames
  srcs[["opts"]] <- opts
  st_geometry(srcs) <- "geometry"
  relocate(srcs, !!"geometry", .after = last_col())
}
