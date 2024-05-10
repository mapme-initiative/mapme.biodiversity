.vector_footprint <- function(src, opts = NULL) {
  info <- sf::gdal_utils("ogrinfo", src, options = c("-json", "-so", "-ro", "-nomd", "-nocount", opts), quiet = TRUE)
  if (length(info) == 0) {
    return(NULL)
  }
  info <- jsonlite::parse_json(info)
  layers <- info[["layers"]]

  layers_bbox <- purrr::map_dfr(layers, function(layer) {
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
  bbox[, "source"]
}

.raster_footprint <- function(src, opts = NULL) {
  info <- sf::gdal_utils("gdalinfo", src, options = c("-json", "-norat", "-noct", "-nomd", opts), quiet = TRUE)
  info <- jsonlite::parse_json(info)
  crs <- st_crs(info[["coordinateSystem"]][["wkt"]])

  bbox <- info[["wgs84Extent"]] %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>%
    read_sf() %>%
    dplyr::mutate(source = src)
  st_crs(bbox) <- st_crs(4326)
  bbox <- st_transform(bbox, crs)

  if (st_is_empty(bbox)) {
    coords <- info$cornerCoordinates
    bbox <- st_bbox(c(
      xmin = coords$lowerLeft[[1]],
      xmax = coords$upperRight[[1]],
      ymin = coords$lowerLeft[[2]],
      ymax = coords$upperLeft[[2]]
    ), crs = crs) %>%
      st_as_sfc() %>%
      st_as_sf() %>%
      dplyr::mutate(source = src)
  }
  bbox[, "source"]
}

.set_precision <- function(data, precision = 1e5) {
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


.get_spds <- function(src, dest, oo = character(0), co = character(0),
                      what = c("vector", "raster"),
                      gdal_conf = NULL) {
  what <- match.arg(what)
  withr::with_envvar(gdal_conf, code = if (spds_exists(dest)) {
    return(TRUE)
  })
  if (is.null(oo)) oo <- character(0)
  if (is.null(co)) co <- character(0)
  opts <- c(oo, co)

  util <- switch(what,
    vector = "vectortranslate",
    raster = "translate"
  )
  withr::with_envvar(gdal_conf, code = {
    if (spds_exists(dest)) {
      return(TRUE)
    }
    try(sf::gdal_utils(
      util = util,
      source = src,
      destination = dest,
      options = opts
    ))
  })
  withr::with_envvar(gdal_conf, code = return(spds_exists(dest)))
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
spds_exists <- function(path,
                        what = c("vector", "raster")) {
  what <- match.arg(what)

  info <- switch(what,
    vector = sf::gdal_utils("ogrinfo", path,
      options = c(
        "-json", "-ro", "-so", "-nomd",
        "-nocount", "-noextent", "-nogeomtype"
      ),
      quiet = TRUE
    ),
    raster = sf::gdal_utils("gdalinfo", path,
      options = c("-json", "-nomd", "-norat", "-noct"),
      quiet = TRUE
    )
  )

  if (length(info) == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
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
#' vec <- system.file("shape/nc.shp", package = "sf")
#' make_footprints(vec, what = "vector")
#'
#' # a raster resource
#' ras <- system.file("ex/elev.tif", package = "terra")
#' make_footprints(ras, what = "raster")
make_footprints <- function(srcs = NULL,
                            filenames = if (inherits(srcs, "sf")) basename(srcs[["source"]]) else basename(srcs),
                            what = c("vector", "raster"),
                            oo = NULL,
                            co = NULL) {
  stopifnot(is.null(oo) || (inherits(oo, "list") | inherits(oo, "character")))
  stopifnot(is.null(co) || (inherits(co, "list") | inherits(co, "character")))
  stopifnot(inherits(srcs, "sf") | inherits(srcs, "character"))
  stopifnot(inherits(filenames, "character") | is.null(filenames))

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
      vector = purrr::map2(srcs, oo, function(src, oo) .vector_footprint(src, oo)),
      raster = purrr::map2(srcs, oo, function(src, oo) .raster_footprint(src, oo)),
      stop("Can make footprints for vector and raster data only.")
    )
    srcs <- purrr::list_rbind(srcs)
  }

  srcs <- st_as_sf(tibble::as_tibble(srcs))
  srcs[["type"]] <- what
  srcs[["filename"]] <- filenames
  srcs[["oo"]] <- oo
  srcs[["co"]] <- co
  st_geometry(srcs) <- "geometry"
  srcs[, c("filename", "type", "oo", "co", "source")]
}
