#' Portfolio methods
#'
#' `write_portfolio()` writes a processed biodiversity portfolio to disk.
#' Portfolio data will only be serialized to disk as a GeoPackage including
#' two tables: `metadata` and `indicators`. The `metadata` tables
#' includes, among other simple variables the geometries and a primary
#' key called `assetid`. The 'indicators' tables includes the foreign key
#' `assetid`, a column called `indicator` giving the name of the original
#' indicator as well as the standard indicator columns `datetime`, `variable`,
#' `unit`, and `value`. For convenience, use `read_portfolio()` to read
#' such a portfolio GeoPackage back into R.
#'
#' @param x A portfolio object processed with `mapme.biodiversity`.
#' @param dsn A file path for the output file (must end with `gpkg`).
#' @param ... Additional arguments supplied to `write_sf()` or `read_sf()`
#' @return `write_portfolio()` returns `dsn`, invisibly.
#' @name portfolio
#' @export
write_portfolio <- function(x,
                            dsn,
                            ...) {
  assetid <- NULL
  x <- .check_portfolio(x)
  inds_cols <- .indicators_col(x)

  if (length(inds_cols) == 0) {
    stop("No calculated indicators have been found")
  }

  if (tools::file_ext(dsn) != "gpkg") {
    warning("Can serialze portoflio only to GeoPackage.")
    dsn <- gsub(tools::file_ext(dsn), "gpkg", dsn)
  }

  metadata <- dplyr::select(x, -dplyr::all_of(names(inds_cols)))
  write_sf(metadata, dsn, "metadata", ...)

  data <- st_drop_geometry(dplyr::select(x, assetid, dplyr::all_of(names(inds_cols))))
  purrr::walk(names(inds_cols), function(ind) {
    tmp <- dplyr::select(data, assetid, dplyr::all_of(ind))
    tmp <- tidyr::unnest(tmp, dplyr::all_of(ind), keep_empty = FALSE)
    tmp[["indicator"]] <- ind
    vars <- c("assetid", "indicator", "datetime", "variable", "unit", "value")
    if (!all(vars %in% names(tmp))) {
      msg <- sprintf("Indicator '%s' contained no valid values. Dropping it from the output.", ind)
      warning(msg)
      return()
    }
    write_sf(tmp[, vars], dsn, "indicators", append = TRUE, ...)
  })

  return(invisible(dsn))
}

#' `read_portfolio()` is used to read a portfolio object that was previously
#' written to disk via `write_portfolio()` back into R as an `sf` object.
#' It should be directed against a GeoPackage which was the output
#' of `write_portfolio()`, otherwise the function is very likely to fail.
#' All available indicators will be read back into R as nested list columns
#' reflecting the output once `calc_indicators()` has been called.
#'
#' @param src A character vector pointing to a GeoPackage that has been
#'   previously written to disk via `write_portfolio()`
#' @return `read_portfolio()` returns an `sf` object object with nested list
#'   columns for every indicator found in the GeoPackage source file.
#' @name portfolio
#' @export
#'
read_portfolio <- function(src, ...) {
  assetid <- indicator <- NULL
  all_layers <- st_layers(src)
  if (!identical(all_layers[["name"]], c("metadata", "indicators"))) {
    stop(sprintf(
      "Input file at '%s' does not seem to be a proper portfolio file written with 'write_portfolio()'",
      src
    ))
  }

  metadata <- read_sf(src, layer = "metadata")
  data <- read_sf(src, layer = "indicators")

  data <- tidyr::nest(data, data = c(-assetid, -indicator))
  is_null <- purrr::map_lgl(data[["data"]], function(x) all(is.na(x)))
  data[["data"]][is_null] <- list(NULL)
  data <- tidyr::pivot_wider(data, id_cols = assetid, names_from = indicator, values_from = data)

  metadata <- dplyr::left_join(metadata, data, by = "assetid")
  .geom_last(st_as_sf(tibble::as_tibble(metadata)))
}

#' Transform portfolio to long
#'
#' `portfolio_long()` transforms a portfolio to long-format, potentially
#' dropping geometries in the process.
#'
#' @param indicators If NULL (the default), all indicator columns will be detected
#'   and transformed automatically. If a character vector is supplied, only
#'   those indicators will be transformed.
#' @param drop_geoms A logical, indicating if geometries should be dropped.
#'
#' @return `portfolio_long()` returns the portfolio object in long-format.
#' @name portfolio
#' @export
#'
portfolio_long <- function(x, indicators = NULL, drop_geoms = FALSE) {
  .check_portfolio(x)
  if (is.null(indicators)) {
    indicators <- names(.indicators_col(x))
  }
  stopifnot(all(indicators %in% names(x)))

  is_null <- sapply(indicators, function(ind) {
    all(sapply(x[[ind]], is.null))
  })

  if (all(is_null)) {
    warning("All indicator columns contained 'NULL'.")
    return(x[, -which(names(x) %in% indicators)])
  }

  if (any(is_null)) {
    x <- x[, -.indicators_col(x)[is_null]]
    indicators <- names(.indicators_col(x))
  }

  if (drop_geoms) x <- st_drop_geometry(x)

  ind_cols <- c("indicator", "datetime", "variable", "unit", "value")
  all_cols <- names(x)
  other_cols <- all_cols[-which(all_cols %in% indicators)]

  x_long <- purrr::map(indicators, function(ind) {
    indicator <- tidyr::unnest(x, cols = {{ ind }})
    indicator["indicator"] <- ind
    indicator <- indicator[, c(other_cols, ind_cols)]
  }) %>%
    purrr::list_rbind()

  if (!drop_geoms) {
    x_long <- .geom_last(st_as_sf(x_long))
  }
  x_long
}

#' Transform portfolio to wide
#'
#' `portfolio_wide()` transforms a portfolio to wide-format, potentially
#' dropping geometries in the process.
#'
#' @return `portfolio_wide()` returns the portfolio object in wide-format.
#' @name portfolio
#' @export
#'
portfolio_wide <- function(x, indicators = NULL, drop_geoms = FALSE) {
  assetid <- NULL
  .check_portfolio(x)
  if (is.null(indicators)) {
    indicators <- names(.indicators_col(x))
  }
  stopifnot(all(indicators %in% names(x)))

  is_null <- sapply(indicators, function(ind) {
    all(sapply(x[[ind]], is.null))
  })

  if (all(is_null)) {
    warning("All indicator columns contained 'NULL'.")
    return(x[, -which(names(x) %in% indicators)])
  }

  if (any(is_null)) {
    x <- x[, -.indicators_col(x)[is_null]]
    indicators <- names(.indicators_col(x))
  }

  if (drop_geoms) x <- st_drop_geometry(x)

  indicators_wide <- purrr::map(indicators, function(indicator) {
    indicator_assets <- x[[indicator]]
    indicator_assets <- purrr::map(indicator_assets, function(asset) {
      if (is.null(asset)) {
        return(NULL)
      }
      asset[["indicator"]] <- indicator
      tidyr::pivot_wider(
        asset,
        names_from = c(indicator, datetime, variable, unit),
        names_sep = "_", values_from = value
      )
    })
    names(indicator_assets) <- x[["assetid"]]
    purrr::list_rbind(indicator_assets, names_to = "assetid")
  })

  indicators_wide <- purrr::reduce(indicators_wide, dplyr::left_join, by = "assetid")
  indicators_wide[["assetid"]] <- as.numeric(indicators_wide[["assetid"]])

  x_wide <- x %>%
    dplyr::select(-{{ indicators }}) %>%
    dplyr::left_join(indicators_wide, by = "assetid")

  if (!drop_geoms) {
    x_wide <- .geom_last(st_as_sf(x_wide))
  }
  x_wide
}

.check_portfolio <- function(x, verbose = mapme_options()[["verbose"]]) {
  stopifnot(inherits(x, "sf"))
  sf_col <- attr(x, "sf_column")

  if (st_crs(x) != st_crs(4326)) {
    message("CRS of x is not EPSG:4326. Attempting to transform.")
    x <- st_transform(x, 4326)
  }
  if (any(!unique(st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Only assets of type 'POLYGON' and 'MULTIPOLYGON' are supported.")
  }
  if (!inherits(x, "tbl_df")) {
    x <- st_as_sf(tibble::as_tibble(x))
  }
  has_assetid <- "assetid" %in% names(x)
  if (!has_assetid) x[["assetid"]] <- 1:nrow(x)
  is_unique <- length(unique(x[["assetid"]])) == nrow(x)
  if (!is_unique) {
    msg <- paste("Found a column named 'assetid' with non-unique identifiers.",
      "Overwritting its values.",
      sep = " "
    )
    message(msg)
    x[["assetid"]] <- 1:nrow(x)
  }
  st_geometry(x) <- sf_col
  x
}

.indicators_col <- function(x) {
  inds <- which(sapply(x, is.list))
  is_sf <- inherits(x, "sf")
  if (is_sf && length(inds) == 1) {
    msg <- paste("No calculated indicators have been found.",
      "Cannot write as a portfolio.",
      sep = " "
    )
    stop(msg)
  }
  if (is_sf) {
    inds[-which(names(inds) == attributes(x)[["sf_column"]])]
  } else {
    inds
  }
}
