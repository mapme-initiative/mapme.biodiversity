.chunk <- function(x, chunk_size) {
  stopifnot(inherits(x, "sf") && "assetid" %in% names(x))
  metadata <- st_drop_geometry(x)
  x <- x[, "assetid"]
  x[["chunked"]] <- FALSE
  st_geometry(x) <- "geometry"

  x <- .split_dateline(x)
  if (!is.null(chunk_size)) {
    x <- .split_multipolygons(x, chunk_size)
    x <- .chunk_geoms(x, chunk_size)
  }
  .finalize_assets(x, metadata)
}

.finalize_assets <- function(x, meta) {
  stopifnot("assetid" %in% names(x) && "assetid" %in% names(meta))
  x <- st_sf(tibble::as_tibble(dplyr::left_join(meta, x, by = "assetid")))
  x$chunked <- NULL
  .geom_last(x)
}

.split_dateline <- function(x) {
  stopifnot(inherits(x, "sf"))
  crosses_dateline <- sapply(st_geometry(x), .crosses_dateline)
  if (any(crosses_dateline)) {
    fix_dt <- st_wrap_dateline(x[which(crosses_dateline), ], options = c("WRAPDATELINE=YES", "DATELINEOFFSET=10"))
    fix_dt <- .cast_to_polygon(fix_dt)
    x <- rbind(x[-which(crosses_dateline), ], fix_dt)
  }
  x
}

.crosses_dateline <- function(geom, offset = 10) {
  stopifnot(inherits(geom, "sfg"))
  bbox <- st_bbox(geom)
  sum <- abs(bbox[[1]]) + abs(bbox[[3]])
  diff <- bbox[[1]] < 0 && bbox[[3]] > 0
  diff && (360 - offset) <= sum
}

.cast_to_polygon <- function(geom) {
  stopifnot(inherits(geom, "sf") && "assetid" %in% names(geom))
  is_poly <- st_geometry_type(geom) == "POLYGON"
  polys <- geom[is_poly, ]
  casted <- suppressWarnings(st_cast(geom[!is_poly, ], "POLYGON"))
  polys <- st_sf(rbind(polys, casted))
  .try_make_valid(polys)
}

.try_make_valid <- function(geom) {
  stopifnot(inherits(geom, "sf"))
  is_invalid <- !st_is_valid(geom)

  if (!all(!is_invalid)) {
    geom[is_invalid, ] <- st_make_valid(geom[is_invalid, ])
    still_invalid <- !st_is_valid(geom[is_invalid, ])
    still_invalid <- which(is_invalid)[still_invalid]

    if (length(still_invalid) > 0) {
      geom <- geom[-still_invalid, ]
    }
  }

  types <- st_geometry_type(geom)
  if (any(types == "GEOMETRYCOLLECTION")) {
    cols <- geom[types == "GEOMETRYCOLLECTION", ]
    cols <- suppressWarnings(st_cast(cols))
    types2 <- st_geometry_type(cols)
    cols <- cols[types2 %in% c("POLYGON", "MULTIPOLYGON"), ]
    geom <- rbind(geom[types != "GEOMETRYCOLLECTION", ], cols)
  }

  geom
}

.split_multipolygons <- function(x, chunk_size) {
  stopifnot(inherits(x, "sf") && "assetid" %in% names(x) && "chunked" %in% names(x))
  is_smaller <- .calc_bbox_areas(x) < chunk_size
  x[["chunked"]][is_smaller] <- TRUE

  if (all(is_smaller)) {
    return(x)
  }

  x_split <- x[!is_smaller, ]
  x_split <- .cast_to_polygon(x_split)
  is_smaller2 <- .calc_bbox_areas(x_split) < chunk_size
  x_split[["chunked"]][is_smaller2] <- TRUE
  rbind(x[is_smaller, ], x_split)
}

.calc_bbox_areas <- function(geom) {
  bboxs <- lapply(st_geometry(geom), function(y) st_as_sfc(st_bbox(y)))
  bboxs <- st_sf(do.call("c", bboxs), crs = st_crs(geom))
  as.numeric(st_area(bboxs) / 10000)
}

.chunk_geoms <- function(x, chunk_size) {
  stopifnot(inherits(x, "sf") && "assetid" %in% names(x) && "chunked" %in% names(x))
  if (all(x[["chunked"]])) {
    return(x)
  }
  x_ok <- NULL
  x_grid <- x[!x[["chunked"]], ]
  if (any(x[["chunked"]])) {
    x_ok <- x[x[["chunked"]], ]
  }
  # only chunks if more than 2x2 cells
  n_cells <- ceiling(sqrt(.calc_bbox_areas(x_grid) / chunk_size))
  to_grid <- n_cells > 2
  if (any(!to_grid)) {
    x_ok <- rbind(x_ok, x_grid[!to_grid, ])
    x_grid <- x_grid[to_grid, ]
  }
  if (any(to_grid)) {
    x_grid <- purrr::map(1:nrow(x_grid), function(i) .make_grid(x_grid[i, ], chunk_size))
    x_grid <- st_sf(purrr::list_rbind(x_grid))
  }
  rbind(x_ok, x_grid)
}

.make_grid <- function(geom, chunk_size) {
  # stopifnot(inherits(geom, "sf") && "assetid" %in% names(geom))
  n <- ceiling(sqrt(.calc_bbox_areas(geom) / chunk_size))
  geom_grid <- st_make_grid(geom, n = n)
  geom_grid <- st_intersection(geom_grid, geom)
  geom_grid <- st_sf(geometry = geom_grid, assetid = geom[["assetid"]], chunked = TRUE)
  .try_make_valid(geom_grid)
}

#' @importFrom stats sd var
.aggregation_fun <- function(agg) {
  stopifnot(agg %in% available_stats)

  switch(agg,
         sum = sum,
         mean = mean,
         median = median,
         sd = sd,
         min = min,
         max = max,
         sum = sum,
         var = var
  )
}

.combine_chunks <- function(data, aggregation = "sum") {
  stat <- NULL
  is_null <- sapply(data, is.null)

  if (all(is_null)) {
    return(NULL)
  }

  data <- data[!is_null]
  if (length(data) == 1) {
    return(data[[1]])
  }

  if (aggregation == "stat") {
    data <- data %>%
      purrr::list_rbind() %>%
      dplyr::mutate(
        stat = purrr::map_chr(strsplit(variable, "_"), function(x) rev(x)[1])
      ) %>%
      dplyr::group_by(datetime, variable, unit) %>%
      dplyr::summarise(value = .aggregation_fun(stat[1])(value, na.rm = TRUE)) %>%
      dplyr::ungroup()
  } else {
    agg <- .aggregation_fun(aggregation)
    data <- data %>%
      purrr::list_rbind() %>%
      dplyr::group_by(datetime, variable, unit) %>%
      dplyr::summarise(value = agg(value, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }
  data
}
