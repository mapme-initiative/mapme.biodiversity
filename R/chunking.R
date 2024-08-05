.crosses_dateline <- function(geom, offset = 10) {
  stopifnot(inherits(geom, "sfg"))
  bbox <- st_bbox(geom)
  sum <- abs(bbox[[1]]) + abs(bbox[[3]])
  diff <- bbox[[1]] < 0 && bbox[[3]] > 0
  diff && (360 - offset) <= sum
}

.split_dateline <- function(x) {
  stopifnot(inherits(x, "sf"))
  crosses_dateline <- sapply(st_geometry(x), .crosses_dateline)
  if (any(crosses_dateline)) {
    fix_dt <- st_wrap_dateline(x[which(crosses_dateline), ])
    fix_dt <- .cast_to_polygon(fix_dt)
    x <- rbind(x[-which(crosses_dateline), ], fix_dt)
  }
  x
}

.split_multipolygons <- function(x, chunk_size) {
  stopifnot(inherits(x, "sf"))
  is_smaller <- .calc_bbox_areas(x) < chunk_size
  x[["chunked"]][is_smaller] <- TRUE

  if (all(is_smaller)) {
    return(x)
  }

  x_split <- .cast_to_polygon(x[!is_smaller, ])
  is_smaller <- .calc_bbox_areas(x_split) < chunk_size
  x_split[["chunked"]][is_smaller] <- TRUE
  x <- rbind(x[is_smaller, ], x_split)
}

.cast_to_polygon <- function(geom) {
  is_poly <- st_geometry_type(geom) == "POLYGON"
  polys <- geom[is_poly, ]
  casted <- suppressWarnings(st_cast(geom[!is_poly, ], "POLYGON"))
  .try_make_valid(st_sf(rbind(polys, casted)))
}

.try_make_valid <- function(geom) {
  is_invalid <- !st_is_valid(geom)
  if (all(!is_invalid)) {
    return(geom)
  }
  geom[is_invalid] <- st_make_valid(geom[is_invalid])
  still_invalid <- !st_is_valid(geom[is_invalid])
  still_invalid <- which(is_invalid)[still_invalid]

  if (length(still_invalid) > 0) {
    geom <- geom[-still_invalid]
  }
  geom
}

.calc_bbox_areas <- function(geom) {
  bboxs <- lapply(st_geometry(geom), function(y) st_as_sfc(st_bbox(y)))
  bboxs <- st_sf(do.call("c", bboxs), crs = st_crs(geom))
  as.numeric(st_area(bboxs) / 10000)
}

.grid_geom <- function(geom, chunk_size) {
  stopifnot("assetid" %in% names(geom))
  n <- ceiling(sqrt(.calc_bbox_areas(geom) / chunk_size))
  geom_grid <- st_make_grid(geom, n = n)
  geom_grid <- st_intersection(geom_grid, geom)
  geom_grid <- .try_make_valid(geom_grid)

  is_collection <- st_geometry_type(geom_grid) == "GEOMETRYCOLLECTION"
  if (any(is_collection)) {
    geom_casted <- st_cast(geom_grid[is_collection])
    geom_casted <- geom_casted[st_geometry_type(geom_casted) %in% c("POLYGON", "MULTIPOLYGON")]
    if (length(geom_casted) > 0) {
      geom_grid <- c(geom_grid[!is_collection], geom_casted)
    }
  }
  st_sf(geometry = geom_grid, assetid = geom[["assetid"]])
}


.finalize_assets <- function(x, meta) {
  x <- st_sf(tibble::as_tibble(dplyr::left_join(meta, x, by = "assetid")))
  .geom_last(x)
}

.chunk <- function(x, chunk_size, min_cells = 2) {
  stopifnot(inherits(x, "sf"))
  stopifnot("assetid" %in% names(x))
  metadata <- st_drop_geometry(x)
  x <- x[, "assetid"]
  x[["chunked"]] <- FALSE
  x <- .split_dateline(x)
  x <- .split_multipolygons(x, chunk_size)



  areas <- .calc_bbox_areas(x)
  smaller_chunk_size <- areas < chunk_size
  x_ok <- rbind(x_ok, x[smaller_chunk_size, ])
  x <- x[!smaller_chunk_size, ]

  if (nrow(x) == 0) {
    return(.finalize_assets(x_ok, metadata))
  }

  # n cells in roughly size of chunks
  n_cells <- ceiling(sqrt(.calc_bbox_areas(x) / chunk_size))
  to_grid <- n_cells > min_cells # only if more than 2x2 cells

  if (all(!to_grid)) {
    x_ok <- rbind(x_ok, x)
    return(.finalize_assets(x_ok, metadata))
  }

  x_ok <- rbind(x_ok, x[!to_grid, ])
  x <- x[to_grid, ]

  x <- purrr::map(1:nrow(x), function(i) .grid_geom(x[i, ], chunk_size = chunk_size))
  x <- st_as_sf(purrr::list_rbind(x))
  .finalize_assets(rbind(x_ok, x), metadata)
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
