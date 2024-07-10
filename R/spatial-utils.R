#### -------------------------Exported utils------------------------------####

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
#' @param oo Either a list or a character vector with opening options (-oo)
#'   of the respective GDAL driver. A list must have equal length of the
#'   input sources, a vector will be recycled.
#' @param what A character vector indicating if the resource is a vector or raster file.
#'
#' @return A logical, TRUE if the file exists, FALSE if it does not.
#' @keywords utils
#' @export
#'
#' @examples
#'
#' # a vector resource
#' vec <- system.file("shape/nc.shp", package = "sf")
#' spds_exists(vec, what = "vector")
#'
#' # a raster resource
#' ras <- system.file("ex/elev.tif", package = "terra")
#' spds_exists(ras, what = "raster")
#'
#' # a non existing file
#' spds_exists("not-here.gpkg", what = "vector")
#'
spds_exists <- function(path, oo = character(0), what = c("vector", "raster")) {
  what <- match.arg(what)
  util <- switch(what,
    vector = "ogrinfo",
    raster = "gdalinfo"
  )
  opts <- switch(what,
    vector = c(
      "-json", "-ro", "-so", "-nomd",
      "-nocount", "-noextent", "-nogeomtype", oo
    ),
    raster = c("-json", "-nomd", "-norat", "-noct", oo)
  )
  if (what == "vector" && sf::sf_extSoftVersion()[["GDAL"]] < "3.7.0") {
    util <- "gdalinfo"
    opts <- oo
  }
  info <- sf::gdal_utils(
    util = util,
    source = path,
    options = opts,
    quiet = TRUE
  )
  length(info) > 0
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
#' @param filenames A character vector indicating the filenames of the source
#'   data sets if they were written to a destionation. Defaults to `basename(srcs)`
#'   in case of character type or `basename(srcs[["source"]])` in case of
#'   an sf object.
#' @param what A character vector indicating if the files are vector or raster
#'   files.
#' @param oo Either a list or a character vector with opening options (-oo)
#'   of the respective GDAL driver. A list must have equal length of the
#'   input sources, a vector will be recycled.
#' @param co Either a list or a character vector with creation options (-co)
#'   of the respective GDAL driver. A list must have equal length of the
#'   input sources, a vector will be recycled.
#' @param precision A numeric indicating the precision of coordinates when
#'   a binary round-trip is done (see `?sf::st_as_binary()`).
#'
#' @return An sf object with a the files sources and the geometry indicating
#'   their spatial footprint.
#' @keywords utils
#' @export
#'
#' @examples
#'
#' # a vector resource
#' # requires GDAL >= 3.7.0
#' if (FALSE) {
#'   vec <- system.file("shape/nc.shp", package = "sf")
#'   make_footprints(vec, what = "vector")
#' }
#'
#' # a raster resource
#' ras <- system.file("ex/elev.tif", package = "terra")
#' make_footprints(ras, what = "raster")
make_footprints <- function(srcs = NULL,
                            filenames = if (inherits(srcs, "sf")) basename(srcs[["source"]]) else basename(srcs),
                            what = c("vector", "raster"),
                            oo = NULL,
                            co = NULL,
                            precision = 1e5) {
  stopifnot(is.null(oo) || (inherits(oo, "list") | inherits(oo, "character")))
  stopifnot(is.null(co) || (inherits(co, "list") | inherits(co, "character")))
  stopifnot(inherits(srcs, "sf") | inherits(srcs, "character"))
  stopifnot(inherits(filenames, "character") | is.null(filenames))
  stopifnot(is.numeric(precision) && length(precision) == 1)

  n <- ifelse(inherits(srcs, "sf"), nrow(srcs), length(srcs))
  if (length(filenames) != n) stop("filenames required to be of equal length of sources.")

  if (inherits(oo, "list") && length(oo) != n) {
    stop("Opening options list is required to be equal length of sources.")
  }

  if (inherits(co, "list") && length(co) != n) {
    stop("Creation options list is required to be equal length of sources.")
  }

  if (inherits(oo, "character") | is.null(oo)) {
    oo <- lapply(seq_len(n), function(i) oo)
  }

  if (inherits(co, "character") | is.null(co)) {
    co <- lapply(seq_len(n), function(i) co)
  }

  if (inherits(srcs, "character")) {
    what <- match.arg(what)
    srcs <- switch(what,
      vector = purrr::map2(srcs, oo, function(src, opt) .vector_footprint(src, opt)),
      raster = purrr::map2(srcs, oo, function(src, opt) .raster_footprint(src, opt)),
      stop("Can make footprints for vector and raster data only.")
    )
    srcs <- purrr::list_rbind(srcs)
  }

  srcs <- st_as_sf(tibble::as_tibble(srcs))
  srcs <- .set_precision(srcs, precision)
  srcs[["location"]] <- srcs[["source"]]
  srcs[["type"]] <- what
  srcs[["filename"]] <- filenames
  srcs[["oo"]] <- oo
  srcs[["co"]] <- co
  st_geometry(srcs) <- "geometry"
  srcs[, c("filename", "location", "type", "oo", "co", "source")]
}

#' Prepare resources for an asset
#'
#' This function reads and crops available resources to the extent of a single
#' asset. Specific resources can be queried. If not supplied (the default), all
#' available resources will be prepared.
#'
#' @param avail_resources A list object of available resources. If NULL (the default),
#'   the available resources will automatically be determined.
#' @param resources A character vector with the resources to be prepared. If it
#'   it is NULL (the default) all available resources will be prepared.
#' @param mode A character indicating the reading mode, e.g. either "portfolio"
#'   (the default) or "asset".
#'
#' @return `prep_resources()` returns a list with prepared vector and raster
#'   resources as `sf` and `SpatRaster`-objects.
#' @name mapme
#' @export
prep_resources <- function(x, avail_resources = NULL, resources = NULL, mode = c("portfolio", "asset")) {
  stopifnot(nrow(x) == 1)
  mode <- match.arg(mode)

  if (is.null(avail_resources)) avail_resources <- .avail_resources()
  if (length(avail_resources) == 0) {
    return(NULL)
  }
  if (is.null(resources)) resources <- names(avail_resources)
  if (!any(resources %in% names(avail_resources))) {
    stop("Some requested resources are not available.")
  }

  out <- purrr::map(resources, function(resource) {
    resource <- avail_resources[[resource]]
    resource_type <- unique(resource[["type"]])
    reader <- switch(resource_type,
      raster = .read_raster,
      vector = .read_vector,
      stop(sprintf("Resource type '%s' currently not supported", resource_type))
    )
    reader(x, resource, mode)
  })
  names(out) <- resources
  out
}


#### -------------------------Vector Utils------------------------------####
.vector_footprint <- function(src, oo = NULL) {
  layers_info <- .vector_info(src, oo)
  if (is.null(layers_info)) {
    return(NULL)
  }

  bboxs <- purrr::map_vec(layers_info, .vector_bbox)
  bbox <- st_as_sf(st_union(bboxs))
  st_geometry(bbox) <- "geometry"
  bbox["source"] <- src
  bbox
}

.vector_info <- function(src, oo) {
  stopifnot(is.character(src) && length(src) == 1)
  stopifnot(is.null(oo) || is.character(oo))

  info <- sf::gdal_utils(
    "ogrinfo",
    source = src,
    options = c("-json", "-so", "-ro", "-nomd", "-nocount", oo),
    quiet = TRUE
  )

  if (length(info) == 0) {
    return(NULL)
  }

  info <- jsonlite::parse_json(info)
  info[["layers"]]
}

.vector_bbox <- function(layer) {
  crs <- layer[["geometryFields"]][[1]][["coordinateSystem"]][["wkt"]]
  crs <- st_crs(crs)
  bbox <- as.numeric(layer[["geometryFields"]][[1]][["extent"]])
  names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  st_as_sfc(st_bbox(bbox, crs = crs))
}

.read_vector <- function(x, tindex, mode = "portfolio") {
  matches <- .get_intersection(x, tindex)

  if (nrow(matches) == 0) {
    warning("No intersection with asset.")
    return(NULL)
  }

  paths <- matches[["location"]]

  vectors <- purrr::map(paths, function(path) {
    tmp <- try(read_sf(path, wkt_filter = st_as_text(st_as_sfc(st_bbox(x)))), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      warning(tmp)
      return(NULL)
    }
    if (nrow(tmp) == 0) {
      return(NULL)
    }
    tmp
  })

  is_null <- unlist(lapply(vectors, is.null))
  vectors <- vectors[!is_null]
  if (length(vectors) == 0) {
    return(NULL)
  }
  names(vectors) <- matches[["filename"]][!is_null]
  vectors
}


#### -------------------------Raster Utils------------------------------####
.raster_footprint <- function(src, oo = NULL) {
  info <- .raster_info(src, oo)
  if (is.null(info)) {
    return(NULL)
  }
  bbox <- .raster_bbox(info)
  bbox[["source"]] <- src
  bbox
}

.raster_info <- function(src, oo = NULL) {
  stopifnot(is.character(src) && length(src) == 1)
  stopifnot(is.null(oo) || is.character(oo))

  info <- sf::gdal_utils(
    "gdalinfo",
    source = src,
    options = c("-json", "-norat", "-noct", "-nomd", oo),
    quiet = TRUE
  )

  if (length(info) == 0) {
    return(NULL)
  }

  jsonlite::parse_json(info)
}

.raster_bbox <- function(info) {
  crs <- st_crs(info[["coordinateSystem"]][["wkt"]])

  bbox <- try(
    {
      poly <- jsonlite::toJSON(info[["wgs84Extent"]], auto_unbox = TRUE)
      bbox <- st_read(poly, quiet = TRUE)
      st_transform(bbox, crs)
    },
    silent = TRUE
  )

  if (inherits(bbox, "try-error") || st_is_empty(bbox)) {
    coords <- info[["cornerCoordinates"]]
    bbox <- st_bbox(c(
      xmin = coords$lowerLeft[[1]],
      xmax = coords$upperRight[[1]],
      ymin = coords$lowerLeft[[2]],
      ymax = coords$upperLeft[[2]]
    ), crs = crs)
    bbox <- st_as_sf(st_as_sfc(bbox))
  }
  bbox
}

.read_raster <- function(x, tindex, mode = "portfolio") {
  x <- st_as_sfc(st_bbox(x))
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

  vrts <- sapply(1:n_timesteps, function(i) tempfile(fileext = ".vrt"))
  if (mode == "asset") on.exit(file.remove(vrts))

  out <- purrr::map2(1:n_timesteps, vrts, function(i, vrt) {
    index <- rep(FALSE, n_timesteps)
    index[i] <- TRUE
    filenames <- names(grouped_geoms[index])
    layer_name <- tools::file_path_sans_ext(basename(filenames[1]))
    tmp <- terra::vrt(filenames, filename = vrt)
    names(tmp) <- layer_name
    tmp
  })
  out <- do.call(c, out)

  # crop the source to the extent of the current polygon
  cropped <- try(terra::crop(out, terra::vect(x), snap = "out"))
  if (inherits(cropped, "try-error")) {
    warning(as.character(cropped))
    return(NULL)
  }
  if (mode == "asset") cropped[] <- terra::values(cropped)
  cropped
}

#### -------------------------Unexported utils------------------------------####
.set_precision <- function(data, precision = 1e2) {
  crs <- st_crs(data)
  geoms <- st_geometry(data)
  geoms <- st_sfc(geoms, precision = precision)
  geoms_binary <- st_as_binary(geoms)
  geoms <- st_as_sfc(geoms_binary)
  st_geometry(data) <- geoms
  st_geometry(data) <- "geometry"
  st_crs(data) <- crs
  .geom_last(data)
}


.get_spds <- function(source = NULL,
                      destination = NULL,
                      opts = NULL,
                      what = c("vector", "raster")) {
  what <- match.arg(what)
  stopifnot(is.character(source) && length(source) == 1)
  stopifnot(is.character(destination) && length(destination) == 1)
  stopifnot(is.null(opts) || is.character(opts))
  if (is.null(opts)) opts <- character(0)

  does_exist <- spds_exists(destination, what = what)
  if (does_exist) {
    return(TRUE)
  }

  util <- switch(what,
    vector = "vectortranslate",
    raster = "translate"
  )
  try(sf::gdal_utils(
    util = util,
    source = source,
    destination = destination,
    options = opts
  ))

  return(spds_exists(destination, what = what))
}

.get_intersection <- function(x, tindex) {
  org <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(org)))

  suppressMessages(targets <- st_intersects(x, tindex, sparse = FALSE))
  tindex[which(colSums(targets) > 0), ]
}
